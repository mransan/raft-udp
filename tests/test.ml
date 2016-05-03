module Raft = Raft_pb
module Udp  = Raft_udp_pb
module Conf = Raft_udp_conf
module Counter = Raft_udp_counter.Counter 
module Perf = Raft_udp_counter.Perf

open Lwt.Infix

module Ext = struct 
  let list_make n v = 
    let rec aux l = function
      | 0 -> l 
      | i -> aux (v::l) (i - 1) 
    in 
    aux [] n  

end (* Ext *)

(* -- Server -- *)

let run_server configuration id logger =

  let get_now =
    (* 
     * For easier logging the time is initialized at the 
     * beginning of the program.
     *)
    let t0 = Unix.gettimeofday () in
    (fun () ->
      Unix.gettimeofday () -. t0
    )
  in
  
  let initial_raft_state = Raft_helper.Follower.create
    ~configuration:configuration.Udp.raft_configuration ~now:(get_now ()) ~id () in

  (*
   * Counters to collect statistics about the
   * various rate of pertinent events. Those
   * rates are updated in the main event thread and
   * printed in the print thread.
   *
   *)
  let raft_msg_received_counter = Counter.make () in
  let raft_msg_sent_counter = Counter.make () in
  let log_counter =
    let initial_counter = initial_raft_state.Raft.log_size in
    Counter.make ~initial_counter ()
  in
  let heartbeat_counter = Counter.make () in
  let append_entries_failure_counter = Counter.make () in

  let msg_perf = Perf.make () in 
  let hb_perf = Perf.make () in 

  let next_raft_message_f =
    Raft_udp_ipc.get_next_raft_message_f_for_server configuration id
  in

  let send_raft_message_f =
    let f = Raft_udp_ipc.get_send_raft_message_f configuration in 
    (fun msg server_id -> 
      Counter.incr raft_msg_sent_counter; 
      Raft_udp_log.print_msg_to_send logger id msg server_id 
      >>= (fun () -> 
        f msg server_id
      )
    ) 
  in

  let send_raft_messages_f requests =
    Lwt_list.map_p (fun (msg, receiver_id) ->
      send_raft_message_f msg receiver_id
    ) requests
    >|= ignore
  in

  (*
   * In order to simulate a new request to the leader by a
   * client application we use a separate thread to populate
   * an [Lwt_mvar.t] value. This variable is populated in a
   * separate Lwt thread. (See add_log_t)
   *)

  let add_log_mvar = Lwt_mvar.create_empty () in
  let add_log () =
    Lwt_mvar.take add_log_mvar >|= (fun () -> `Add_log)
  in

  let rec server_loop raft_state now' timeout timeout_type =
    (*
     * [now'] is the time associated with [timeout] meanding that the
     * time deadline at which the [Timeout] exception will be raised should be
     * [now' + timeout].
     *
     *)

    Lwt.catch (fun () ->
      Lwt.pick [
        add_log ();
        Lwt_unix.timeout timeout;
        next_raft_message_f ()
      ];
    ) (function
        | Lwt_unix.Timeout -> Lwt.return `Timeout
          (*
           * [Lwt_unix.Timout] exception is raised by the [Lwt_unix.timeout]
           * thread after the specfied timeout.
           *
           * Since timeout event is part of main event (ie not an exception),
           * we transform it here to a proper server event [`Timeout].
           *
           *)
        | _                -> Lwt.return `Failure
    )
    >>=(fun event ->

      let handle_follow_up_action raft_state =   
        let now = get_now () in
        let {Raft.timeout; timeout_type } = Raft_helper.Timeout_event.next raft_state now in
        server_loop raft_state now timeout timeout_type 
      in
      
      let now = get_now () in

      Counter.set log_counter raft_state.Raft.log_size;

      match event with
      | `Raft_message msg -> (
        Counter.incr raft_msg_received_counter;
        begin match msg with
        | Raft.Append_entries_response {Raft.result = Raft.Failure ; _} ->
          Counter.incr append_entries_failure_counter
        | _ -> ()
        end;

        (*
        Format.printf "--------------------------------------------\n";
        Format.printf ">> Message received: %a\n%!" Raft.pp_message msg;
        *)
        let raft_state, responses = Perf.f3 msg_perf 
          Raft_logic.Message.handle_message raft_state msg now
        in
        (*
        Format.printf ">> New state: %a\n%!" Raft.pp_state raft_state;
        Format.printf ">> action: %a\n%!" Raft.pp_follow_up_action action;
        *)
        Raft_udp_log.print_msg_received logger msg id
        >>= (fun () -> 
          send_raft_messages_f responses
        )
        >>=(fun _ -> handle_follow_up_action raft_state)
      )

      | `Timeout -> (
        let raft_state, msgs = match timeout_type with
          | Raft.Heartbeat -> (
            Counter.incr heartbeat_counter;
            Perf.f2 hb_perf 
              Raft_logic.Message.handle_heartbeat_timeout raft_state now
          )

          | Raft.New_leader_election -> (
            print_endline "NEW LEADER ELECTION%!";
            Raft_logic.Message.handle_new_election_timeout raft_state now
          )

        in
        send_raft_messages_f msgs
        >>=(fun _ -> handle_follow_up_action raft_state)
      )

      | `Failure -> (
        Lwt_io.eprintf "System failure...\n%!"
      )

      | `Add_log -> (

        let new_log_response  = 
          let data  = Bytes.of_string (string_of_float now) in 
          let datas = Ext.list_make 20 data in 
          Raft_logic.Message.handle_add_log_entries raft_state datas now 
        in 

        match new_log_response with
        | Raft_logic.Message.Delay
        | Raft_logic.Message.Forward_to_leader _ -> 
          server_loop raft_state now (timeout +. now' -. now) timeout_type
        | Raft_logic.Message.Appended (raft_state, msgs) -> 
          send_raft_messages_f msgs
          >>=(fun _ -> handle_follow_up_action raft_state)
      )
    )
  in

  let server_t =
    let {
      Raft.timeout; 
      timeout_type
    } = Raft_helper.Timeout_event.next initial_raft_state (get_now ()) in 
    server_loop initial_raft_state (get_now ()) timeout timeout_type 
  in

  let add_log_t  =
    let rec aux () =
      Lwt_unix.sleep 0.001
      >>=(fun () ->
        Lwt_mvar.put add_log_mvar ()
      )
      >>=aux
    in
    aux ()
  in

  let print_stats_t =
    
    let print_header_every = 20 in 

    let rec aux counter () =
      Lwt_unix.sleep 0.2
      >>=(fun () -> 
        if counter = 0 
        then 
          Lwt_io.printf 
            " %15s | %15s | %10s | %8s | %4s | %6s |\n" 
            "recv msg/s"
            "sent msg/s" 
            "log/s"
            "log nb"
            "hb/s"
            "er/s"
          >|=(fun () -> print_header_every)
        else
          Lwt.return counter 
      )
      >>=(fun counter ->
        Lwt_io.printf " %15.3f | %15.3f | %10.3f | %8i | %4.1f | %6.1f |  \n"
          (Counter.rate raft_msg_received_counter)
          (Counter.rate raft_msg_sent_counter)
          (Counter.rate log_counter)
          (Counter.value log_counter)
          (Counter.rate heartbeat_counter)
          (Counter.rate append_entries_failure_counter)
        >>=(fun () -> aux (counter - 1) ()) 
      )
    in
    aux 0 () 
  in

  Lwt.join [
    server_t;
    print_stats_t;
    add_log_t;
  ]

let run configuration id = 
  let t = 
    let file_name = Printf.sprintf "raft_upd_%i.log" id in 
    Lwt_log.file ~mode:`Truncate ~file_name () 
    >>=(fun logger -> 
      run_server configuration id logger 
    ) 
  in
  Lwt_main.run t 

let () =
  Random.self_init ();
  let configuration = Conf.default_configuration () in

  let id   = ref (-1) in
  let id_spec = Arg.Symbol (["0";"1";"2"], fun s ->
    id := int_of_string s
  ) in

  Arg.parse [
    ("--id", id_spec , " : server raft id");
  ] (fun _ -> ()) "test.ml";


  run configuration !id
