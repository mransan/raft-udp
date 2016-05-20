module Raft  = Raft_pb
module Udp   = Raft_udp_pb
module Conf  = Raft_udp_conf
module Perf  = Raft_udp_counter.Perf
module Stats = Raft_udp_serverstats

open Lwt.Infix

open Lwt_log_core

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

  let stats = 
    Stats.make ~initial_log_size:initial_raft_state.Raft.log_size ()
  in

  let next_raft_message_f =
    Raft_udp_ipc.get_next_raft_message_f_for_server configuration id
  in

  let send_raft_message_f =
    let f = Raft_udp_ipc.get_send_raft_message_f configuration in 
    fun msg server_id -> 
      Stats.tick_raft_msg_send stats; 
      Raft_udp_log.print_msg_to_send logger id msg server_id 
      >>= (fun () -> f msg server_id)
  in

  let send_raft_messages_f raft_state requests =
    Lwt_list.map_p (fun (msg, receiver_id) ->
      send_raft_message_f msg receiver_id
    ) requests
    >|= ignore
  in

  let next_client_request, send_response_f = 
    let req_stream, send_response_f = 
      Raft_udp_clientipc.client_request_stream logger configuration stats id 
    in 
    (
      (fun () -> Lwt_stream.next req_stream >|= (fun x -> `Client_request x)), 
      (* TODO Replace by get *)
      send_response_f
    )
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
        Lwt_unix.timeout timeout;
        next_raft_message_f ();
        next_client_request();
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

      Stats.set_log_count stats raft_state.Raft.log_size;

      match event with
      | `Raft_message msg -> (
        Lwt_unix.sleep 0.00
        >>=(fun () ->
        log ~logger ~level:Notice ">> Raft Message Received"
        )
        >>=(fun () ->
          Raft_udp_log.print_state logger raft_state
        )
        >>=(fun () ->
          Stats.tick_raft_msg_recv stats;
          begin match msg with
          | Raft.Append_entries_response {Raft.result = Raft.Log_failure  _ ; _}
          | Raft.Append_entries_response {Raft.result = Raft.Term_failure ; _} ->
            Stats.tick_append_entries_failure stats; 
          | _ -> ()
          end;

          let raft_state, responses, notifications = Perf.f3 (Stats.msg_processing stats) 
            Raft_logic.handle_message raft_state msg now
          in
          (* TODO Handle notifications *)
          Raft_udp_log.print_msg_received logger msg id
          >>= (fun () -> 
            send_raft_messages_f raft_state responses
          )
          >>=(fun _ -> handle_follow_up_action raft_state)
        )
      )

      | `Timeout -> (
        begin match timeout_type with
        | Raft.Heartbeat -> (
          Stats.tick_heartbeat stats;
          log ~logger ~level:Notice ">> Heartbeat timeout" 
          >|= (fun () ->
            Perf.f2 (Stats.hb_processing stats)
              Raft_logic.handle_heartbeat_timeout raft_state now
          )
          >|= (fun (raft_state, msgs) -> (raft_state, msgs, []))
        )

        | Raft.New_leader_election -> (
          print_endline "NEW LEADER ELECTION%!";
          log ~logger ~level:Notice ">> Leader Election timeout"
          >|= (fun () ->
            Raft_logic.handle_new_election_timeout raft_state now
          )
        )
        end

        >>=(fun (raft_state, msgs, notifications) ->
          (* TODO Handle notifications *)
          send_raft_messages_f raft_state msgs
          >|= (fun _ -> raft_state) 
        )

        >>=(fun raft_state -> handle_follow_up_action raft_state)
      )

      | `Failure -> (
        Lwt_io.eprintf "System failure...\n%!"
      )

      | `Client_request (client_request, handle) -> (

        match client_request with
        | Udp.Ping {Udp.request_id; } -> 
          begin  
            let client_response = Udp.(Pong {
              request_id; 
              leader_id = Raft_helper.State.current_leader raft_state; 
            }) in 
            send_response_f (Some (client_response, handle)); 
            handle_follow_up_action raft_state
          end 

        | Udp.Add_log {Udp.request_id; data} -> 
          
          let new_log_response  = 
            let datas = [(data, request_id)] in 
            Raft_logic.handle_add_log_entries raft_state datas now 
          in 

          match new_log_response with
          | Raft_logic.Delay
          | Raft_logic.Forward_to_leader _ -> 
            begin 
              let client_response = Udp.(Add_log_not_a_leader {
                leader_id = Raft_helper.State.current_leader raft_state; 
              }) in 
              send_response_f (Some (client_response, handle)); 
              handle_follow_up_action raft_state
            end

          | Raft_logic.Appended (raft_state, msgs) -> 
            begin 
              send_response_f (Some (Udp.Add_log_success, handle)); 
              log_f ~logger ~level:Notice ">> Log Added (log size: %i)" raft_state.Raft_pb.log_size 
              >>=(fun () ->
                send_raft_messages_f raft_state msgs
              )
              >>=(fun _ -> handle_follow_up_action raft_state)
            end
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

  (* 
  let add_log_t  =
    let rec aux () =
      Lwt_unix.sleep 0.0005
      >>=(fun () ->
        Lwt_mvar.put add_log_mvar ()
      )
      >>=aux
    in
    aux ()
  in
  *)

  Lwt.join [
    server_t;
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
  Printf.printf ">>PID: %i\n%!" (Unix.getpid ());
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
