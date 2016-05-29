module RPb = Raft_pb
module RHelper = Raft_helper
module RRole = Raft_role

module Pb = Raft_udp_pb
module Conf = Raft_udp_conf
module Counter = Raft_udp_counter
module Server_stats = Raft_udp_serverstats
module Ipc = Raft_udp_ipc
module Server_ipc = Raft_udp_serveripc
module Client_ipc = Raft_udp_clientipc
module Compaction = Raft_udp_compaction
module Log_record = Raft_udp_logrecord

open Lwt.Infix

open Lwt_log_core

(* -- Server -- *)

let run_server configuration id logger print_header slow =

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
  
  let initial_raft_state = RRole.Follower.create
    ~configuration:configuration.Pb.raft_configuration ~now:(get_now ()) ~id () in

  let stats = 
    let print_header = if print_header then Some () else None in
    Server_stats.make 
      ?print_header 
      ~initial_log_size:initial_raft_state.RPb.log_size 
      ~id ()
  in

  let send_raft_message =
    let ipc_handle, f = Ipc.get_send_raft_message_f configuration in 
    fun ((msg, server_id) as msg_to_send) -> 
      Server_stats.tick_raft_msg_send stats; 
      Raft_udp_log.print_msg_to_send logger id msg server_id 
      >|= (fun () -> f ipc_handle msg_to_send)
  in

  let send_raft_messages requests =
    Lwt_list.map_p (fun msg_to_send ->
      send_raft_message msg_to_send
    ) requests
    >|= ignore
  in
  
  let module Event = struct 
    type e  = 
      | Failure        of string  
      | Raft_message   of Raft_pb.message 
      | Client_request of Client_ipc.client_request
      | Timeout        of Raft_pb.timeout_event_time_out_type  
      | Compaction_initiate
      | Compaction_update  of RPb.log_interval list 

    type threads = {
      next_raft_message_t : e Lwt.t;
      next_client_request_t : e Lwt.t;
      next_timeout_t : e Lwt.t;
      compaction_t : e Lwt.t;
    }

    let list_of_threads threads =
      let {
        next_raft_message_t;
        next_client_request_t;
        next_timeout_t;
        compaction_t;
      } = threads in 
      next_raft_message_t   :: 
      next_client_request_t ::
      next_timeout_t        ::
      compaction_t          :: []

  end in
  
  let get_next_raft_message =
    let f = Ipc.get_next_raft_message_f_for_server configuration id in
    fun () ->
      f ()
      >|= (function
        | Ipc.Failure        -> Event.Failure "Raft IPC"
        | Ipc.Raft_message x -> Event.Raft_message x
      ) 
  in

  let get_next_client_request, send_response = 
    let req_stream, send_response = 
      Client_ipc.client_request_stream logger configuration stats id 
    in 
    let get_next_client_request () = 
      Lwt_stream.get req_stream 
       >|= (function 
         | None   -> Event.Failure "Client IPC" 
         | Some r -> Event.Client_request r
       )
    in 
    (get_next_client_request, send_response)
  in 

  let send_responses client_responses =
    List.iter (fun res -> send_response (Some res)) client_responses 
  in

  let get_next_timeout timeout timeout_type = 
    Lwt_unix.sleep timeout >|= (fun () -> Event.Timeout timeout_type)
  in 

  let get_next_compaction () = 
    Lwt_unix.sleep configuration.Pb.compaction_period
    >|=(fun () -> Event.Compaction_initiate)
  in

  let rec server_loop threads state =
    (*
     * [now'] is the time associated with [timeout] meanding that the
     * time deadline at which the [Timeout] exception will be raised should be
     * [now' + timeout].
     *
     *)

    Lwt.nchoose (Event.list_of_threads threads) 

    >>=(fun events -> 

      let handle_follow_up_action threads  ({Server_ipc.raft_state;_} as state) = 
        let now = get_now () in
        let {RPb.timeout; timeout_type } = RHelper.Timeout_event.next raft_state now in
        Lwt.cancel threads.Event.next_timeout_t;
        let threads = {threads with 
          Event.next_timeout_t = get_next_timeout timeout timeout_type
        } in 
        server_loop threads state 
      in
      
      let now = get_now () in
      
      Server_stats.set_log_count stats state.Server_ipc.raft_state.RPb.log_size;

      Lwt_list.fold_left_s (fun (state, threads) event -> 
        match event with
        | Event.Raft_message msg -> (
          begin 
            if slow 
            then Lwt_unix.sleep  0.1
            else Lwt.return_unit 
          end
          >>=(fun () ->
            Server_ipc.handle_raft_message ~logger ~stats ~now state msg 
          )
          >>=(fun (state, msg_to_send, client_responses) ->
            send_responses client_responses;
            send_raft_messages msg_to_send
            >|=(fun () -> 
              let threads = { threads with
                Event.next_raft_message_t = get_next_raft_message ();
              } in
              (state, threads))
          )
        )

        | Event.Timeout timeout_type -> (
          Server_ipc.handle_timeout ~logger ~stats ~now state timeout_type
          >>=(fun (state, msg_to_send, client_responses) ->
            send_responses client_responses;
            send_raft_messages msg_to_send 
            >|=(fun () -> (state, threads))
          )
        )

        | Event.Failure context -> (
          Printf.eprintf "Exiting, context: %s\n" context; 
          exit 1
        )

        | Event.Client_request client_request -> (
          Server_ipc.handle_client_request ~logger ~stats ~now state client_request
          >>=(fun (state, msg_to_send, client_responses) ->
            send_responses client_responses;
            send_raft_messages msg_to_send 
            >|=(fun () -> 
              let threads = { threads with
                Event.next_client_request_t = get_next_client_request ();
              } in
              (state, threads))
          )
        )
        | Event.Compaction_initiate -> (
          let {Server_ipc.raft_state; _ } = state in 
          let compaction_t = 
            Compaction.perform_compaction logger configuration raft_state  
            >|=(fun compacted_intervals -> 
              Event.Compaction_update compacted_intervals
            )
          in
          let threads = { threads with Event.compaction_t } in 
          Lwt.return (state, threads)
        )
        | Event.Compaction_update compacted_intervals -> (
          let {Server_ipc.raft_state; _ } = state in 
          Compaction.update_state logger compacted_intervals raft_state 
          >|=(fun raft_state -> 
            let threads = {threads with Event.compaction_t = get_next_compaction ()} in 
            ({state with Server_ipc.raft_state}, threads)
          )
        ) 
      ) (state, threads) events 
      >>= (fun (state, threads) -> handle_follow_up_action threads state) 
    )
  in

  let {
    RPb.timeout; 
    timeout_type
  } = RHelper.Timeout_event.next initial_raft_state (get_now ()) in 

  let initial_threads = Event.({
    next_client_request_t = get_next_client_request ();
    next_raft_message_t = get_next_raft_message ();
    next_timeout_t = get_next_timeout timeout timeout_type;
    compaction_t = get_next_compaction ();
  }) in 
  
  Log_record.make logger configuration
  >>=(fun handle ->
    server_loop initial_threads Server_ipc.({
      raft_state = initial_raft_state; 
      connection_state = initialize();
      log_record_handle = handle;
    })
  )


let run configuration id print_header slow log = 
  let t = 
    begin 
      if log
      then 
        let file_name = Printf.sprintf "raft_upd_%i.log" id in 
        Lwt_log.file ~mode:`Truncate ~file_name () 
      else 
        Lwt.return Lwt_log_core.null 
    end
    >>=(fun logger -> 
      run_server configuration id logger print_header slow 
    ) 
  in
  Lwt_main.run t 

let () =
  Printf.printf ">>PID: %i\n%!" (Unix.getpid ());
  Random.self_init ();
  let configuration = Conf.default_configuration () in
  let nb_of_servers = List.length configuration.Pb.servers_udp_configuration in 

  let ids = 
    let rec aux acc = function
      | -1 -> acc
      | n  -> aux ((string_of_int n)::acc) (n - 1)
    in 
    aux [] (nb_of_servers - 1)
  in

  let id   = ref (-1) in
  let id_spec = Arg.Symbol (ids, fun s ->
    id := int_of_string s
  ) in

  let print_header = ref false in 
  let print_header_spec = Arg.Set print_header in
  
  let slow = ref false in 
  let slow_spec = Arg.Set slow  in
  
  let log = ref false in 
  let log_spec = Arg.Set log  in

  Arg.parse [
    ("--id", id_spec , " : server raft id");
    ("--print-header", print_header_spec, " : enable header printing");
    ("--slow", slow_spec, " : make server slow");
    ("--log", log_spec, " : enable logging");
  ] (fun _ -> ()) "test.ml";

  run configuration !id !print_header !slow !log
