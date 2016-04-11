module Raft = Raft_pb
module Udp  = Raft_udp_pb
module Conf = Raft_udp_conf

open Lwt.Infix

let new_election_timeout ?timeout {Udp.raft_configuration; _ } =
  let timeout = match timeout with
    | Some timeout -> timeout
    | None ->
      let {Raft.election_timeout; _ } =  raft_configuration in
      election_timeout
  in
  let range  = raft_configuration.Raft.election_timeout_range in
  timeout +. (Random.float range -. (range /. 2.))

let timeout_of_wait_rpc configuration {Raft_pb.timeout; timeout_type} =
  match timeout_type with
  | Raft.Heartbeat -> timeout
  | Raft.New_leader_election -> new_election_timeout ~timeout configuration

(* -- Server -- *)

let run_server configuration id =

  let now =
    (* 
     * For easier logging the time is initialized at the 
     * beginning of the program.
     *)
    let t0 = Unix.gettimeofday () in
    (fun () ->
      Unix.gettimeofday () -. t0
    )
  in

  let next_raft_message_f =
    Raft_udp_ipc.get_next_raft_message_f_for_server configuration id
  in

  let send_raft_message_f =
    Raft_udp_ipc.get_send_raft_message_f configuration
  in

  let send_raft_messages_f requests =
    Lwt_list.map_p (fun (msg, receiver_id) ->
      send_raft_message_f msg receiver_id
    ) requests
    >|= ignore
  in

  let initial_raft_state = Raft_helper.Follower.create
    ~configuration:configuration.Udp.raft_configuration ~id () in

  (*
   * Counters to collect statistics about the
   * various rate of pertinent events. Those
   * rates are updated in the main event thread and
   * printed in the print thread.
   *
   *)
  let raft_msg_counter = Raft_udp_counter.make () in
  let log_counter =
    let initial_counter = initial_raft_state.Raft.log_size in
    Raft_udp_counter.make ~initial_counter ()
  in
  let heartbeat_counter = Raft_udp_counter.make () in
  let append_entries_failure_counter = Raft_udp_counter.make () in

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

      let now = now () in

      let handle_follow_up_action raft_state = function
        | Raft.Wait_for_rpc ({Raft.timeout_type; } as wait_for_rpc) ->
          server_loop raft_state now (timeout_of_wait_rpc configuration wait_for_rpc) timeout_type
      in

      Raft_udp_counter.set log_counter raft_state.Raft.log_size;

      match event with
      | `Raft_message msg -> (
        Raft_udp_counter.incr raft_msg_counter;
        begin match msg with
        | Raft.Append_entries_response {Raft.result = Raft.Failure ; _} ->
          Raft_udp_counter.incr append_entries_failure_counter
        | _ -> ()
        end;

        (*
        Format.printf "--------------------------------------------\n";
        Format.printf ">> Message received: %a\n%!" Raft.pp_message msg;
        *)
        let raft_state, responses, action = Raft_logic.Message.handle_message raft_state msg now in
        (*
        Format.printf ">> New state: %a\n%!" Raft.pp_state raft_state;
        Format.printf ">> action: %a\n%!" Raft.pp_follow_up_action action;
        *)
        send_raft_messages_f responses
        >>=(fun _ -> handle_follow_up_action raft_state action)
      )

      | `Timeout -> (
        let raft_state, msgs, action = match timeout_type with
        | Raft.Heartbeat -> (
          Raft_udp_counter.incr heartbeat_counter;
          Raft_logic.Message.handle_heartbeat_timeout raft_state now
        )
        | Raft.New_leader_election -> (
          Raft_logic.Message.handle_new_election_timeout raft_state now
        )
        in
        send_raft_messages_f msgs
        >>=(fun _ -> handle_follow_up_action raft_state action)
      )

      | `Failure -> (
        Lwt_io.eprintf "System failure...\n%!"
      )

      | `Add_log -> (
        let raft_state = match raft_state.Raft.role with
          | Raft.Leader _ ->
            let data = Bytes.of_string (string_of_float now) in
            Raft_helper.Leader.add_log data raft_state
            |>Raft_helper.Leader.add_log data
            |>Raft_helper.Leader.add_log data
          | _ -> raft_state
        in
        server_loop raft_state now (timeout +. now' -. now) timeout_type
      )
    )
  in

  let server_t =
    let timeout = new_election_timeout configuration in
    server_loop initial_raft_state (now ()) timeout Raft.New_leader_election
  in

  let add_log_t  =
    let rec aux () =
      Lwt_unix.sleep 0.0002
      >>=(fun () ->
        Lwt_mvar.put add_log_mvar ()
      )
      >>=aux
    in
    aux ()
  in

  let print_stats_t =
    let rec aux () =
      Lwt_unix.sleep 1.
      >>=(fun () ->
        Lwt_io.printf " %10.3f    | %10.3f  | %8i | %10.3f | %10.3f |  \n"
          (Raft_udp_counter.rate raft_msg_counter)
          (Raft_udp_counter.rate log_counter)
          (Raft_udp_counter.value log_counter)
          (Raft_udp_counter.rate heartbeat_counter)
          (Raft_udp_counter.rate append_entries_failure_counter)
      )
      >>= aux
    in
    Lwt_io.printf "  raft msg/s   |   log/s     |  nb log  |   hb/s     | failures   |\n"
    >>= aux
  in

  Lwt_main.run (Lwt.join [
    server_t;
    print_stats_t;
    add_log_t;
  ])

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

  run_server configuration !id
