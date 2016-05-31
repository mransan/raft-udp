open Lwt.Infix
open Lwt_log_core

module RPb = Raft_pb
module RHelper = Raft_helper
module RRole = Raft_role
module RRev_log_cache = Raft_revlogcache 

module Pb = Raft_udp_pb
module Conf = Raft_udp_conf
module Counter = Raft_udp_counter
module Server_stats = Raft_udp_serverstats
module Ipc = Raft_udp_ipc
module Server_ipc = Raft_udp_serveripc
module Client_ipc = Raft_udp_clientipc
module Compaction = Raft_udp_compaction
module Log_record = Raft_udp_logrecord

let section = Section.make (Printf.sprintf "%10s" "server")

(* -- Server -- *)
  
let get_now =
  (* 
   * For easier logging the time is initialized at the 
   * beginning of the program.
   *)
  let t0 = Unix.gettimeofday () in
  (fun () -> Unix.gettimeofday () -. t0)


module Event = struct 
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

end 

let get_send_raft_messages_f logger stats configuration id = 
  let ipc_handle, f = Ipc.get_send_raft_message_f configuration in 

  fun requests -> 
    Lwt_list.map_p (fun ((msg, server_id) as msg_to_send) ->
      Server_stats.tick_raft_msg_send stats; 
      Raft_udp_log.print_msg_to_send logger section id msg server_id 
      >|= (fun () -> f ipc_handle msg_to_send)
    ) requests
    >|= ignore

let get_next_raft_message_f configuration id =
  let f = Ipc.get_next_raft_message_f_for_server configuration id in
  fun () ->
    f ()
    >|= (function
      | Ipc.Failure        -> Event.Failure "Raft IPC"
      | Ipc.Raft_message x -> Event.Raft_message x
    ) 

let get_client_ipc_f logger stats configuration id = 
  let req_stream, send_client_response = 
    Client_ipc.make logger configuration stats id 
  in 
  let next_client_request () = 
    Lwt_stream.get req_stream 
     >|= (function 
       | None   -> Event.Failure "Client IPC" 
       | Some r -> Event.Client_request r
     )
  in 

  let send_client_responses = fun client_responses -> 
    List.iter (fun response -> 
      send_client_response (Some response)
    ) client_responses 
  in

  (next_client_request, send_client_responses)
  
let next_timeout timeout timeout_type = 
  Lwt_unix.sleep timeout 
  >|= (fun () -> Event.Timeout timeout_type)

let get_next_compaction_f configuration = fun () ->
  Lwt_unix.sleep configuration.Pb.compaction_period
  >|=(fun () -> Event.Compaction_initiate)

let run_server configuration id logger print_header slow =

  let stats = 
    let print_header = if print_header then Some () else None in
    Server_stats.make 
      ?print_header 
      ~initial_log_size:0
      ~id ()
  in

  let send_raft_messages = get_send_raft_messages_f logger stats configuration id in
  let next_raft_message  = get_next_raft_message_f configuration id in 
  
  let (
    next_client_request, 
    send_client_responses
  ) = get_client_ipc_f logger stats configuration id in 

  let next_compaction = get_next_compaction_f configuration in 

  let rec server_loop threads state =

    Lwt.nchoose (Event.list_of_threads threads) 

    >>=(fun events -> 

      let handle_follow_up_action threads  ({Server_ipc.raft_state;_} as state) = 
        let now = get_now () in
        let {RPb.timeout; timeout_type } = RHelper.Timeout_event.next raft_state now in
        Lwt.cancel threads.Event.next_timeout_t;
        let threads = {threads with 
          Event.next_timeout_t = next_timeout timeout timeout_type
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
            send_client_responses client_responses;
            send_raft_messages msg_to_send
            >|=(fun () -> 
              let threads = { threads with
                Event.next_raft_message_t = next_raft_message ();
              } in
              (state, threads))
          )
        )

        | Event.Timeout timeout_type -> (
          Server_ipc.handle_timeout ~logger ~stats ~now state timeout_type
          >>=(fun (state, msg_to_send, client_responses) ->
            send_client_responses client_responses;
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
            send_client_responses client_responses;
            send_raft_messages msg_to_send 
            >|=(fun () -> 
              let threads = { threads with
                Event.next_client_request_t = next_client_request  ();
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
            let threads = {threads with Event.compaction_t = next_compaction  ()} in 
            ({state with Server_ipc.raft_state}, threads)
          )
        ) 
      ) (state, threads) events 
      >>= (fun (state, threads) -> handle_follow_up_action threads state) 
    )
  in

  let initial_raft_state = RRole.Follower.create
    ~configuration:configuration.Pb.raft_configuration 
    ~now:(get_now ()) 
    ~id () 
  in

  let {
    RPb.timeout; 
    timeout_type
  } = RHelper.Timeout_event.next initial_raft_state (get_now ()) in 

  let initial_threads = Event.({
    next_client_request_t = next_client_request ();
    next_raft_message_t = next_raft_message  ();
    next_timeout_t = next_timeout timeout timeout_type;
    compaction_t = next_compaction ();
  }) in 

  Compaction.load_previous_log_intervals logger configuration id 
  >|=(fun log_intervals ->
    RRev_log_cache.from_list log_intervals 
  ) 
  >>=(fun global_cache -> 
    let section = Section.make (Printf.sprintf "%10s" "Recovery") in 
    let from = RRev_log_cache.last_cached_index global_cache in 
    log_f ~logger ~level:Notice ~section "Global cache done, last cached index: %i" from 
    >>=(fun () ->
      Log_record.read_log_records configuration id (fun (log, log_size) ({RPb.index; _ } as log_entry) -> 
        if index >= from 
        then (log_entry::log, log_size + 1) 
        else (log, log_size + 1)
      ) ([], 0)
    )
    >>= (fun (log, log_size) -> 
      let commit_index = match log with
        | [] -> 0 
        | {RPb.index; _ } :: _ -> index 
      in 
      log_f ~logger ~level:Notice ~section "Log read done, commit index: %i" commit_index
      >|=(fun () ->
        {initial_raft_state with RPb.log; log_size; global_cache;commit_index}
      )
    )
  )
  >>=(fun initial_raft_state ->
    Log_record.make logger configuration id 
    >>=(fun handle ->
      server_loop initial_threads Server_ipc.({
        raft_state = initial_raft_state; 
        connection_state = initialize();
        log_record_handle = handle;
      })
    )
  )

let run configuration id print_header slow log = 
  let t = 
    begin 
      if log
      then 
        let file_name = Printf.sprintf "raft_upd_%i.log" id in 
        let template  = "$(date).$(milliseconds) [$(level)] [$(section)] : $(message)" in
        Lwt_log.file ~mode:`Truncate ~template ~file_name () 
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
