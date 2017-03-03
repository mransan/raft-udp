open Lwt.Infix
open !Lwt_log_core

module RPb = Raft_pb
module RHelper = Raft_helper
module RRole = Raft_role
module RLog = Raft_log
module RTypes = Raft_types
module RConv = Raft_pb_conv

module APb = Raft_com_pb
module Conf = Raft_com_conf
module Server_stats = Raft_srv_serverstats
module Raft_logic = Raft_srv_logic
module Client_ipc = Raft_srv_clientipc
module Log_record = Raft_srv_logrecord
module App_ipc = Raft_srv_appipc
module Rate_limiter = Raft_utl_ratelimiter

let section = Section.make (Printf.sprintf "%10s" "server")

(* -- Server -- *)
  
let get_now =
  (* Mtime guarantees monotomic time which is a requirement from 
   * the RAFT library *)
  let t0 = Mtime.counter () in 
  (fun () -> Mtime.(count t0 |> to_s)) 

module Event = struct 
  type e  = 
    | Failure        of string  
      (** Fatal failure, the server will exit *)

    | Raft_message   of Raft_pb.message 
      (** A RAFT message was received from one of the other RAFT servers *)

    | Client_request of Client_ipc.client_request list
      (** A Client request was received *)

    | Timeout        of Raft_types.timeout_type
      (** RAFT Protocol timeout happened *)

    | App_response   of APb.app_response 
      (** A response was received from the App server *)

  type threads = {
    next_raft_message_t : e Lwt.t;
    next_client_request_t : e Lwt.t;
    next_timeout_t : e Lwt.t;
    next_app_reponse_t : e Lwt.t;
  }

  let list_of_threads threads =
    let {
      next_raft_message_t;
      next_client_request_t;
      next_timeout_t;
      next_app_reponse_t;
    } = threads in 
    next_raft_message_t   :: 
    next_client_request_t ::
    next_timeout_t        ::
    next_app_reponse_t    :: []

  let next_timeout raft_state = 
    let now = get_now () in

    let {
      RTypes.timeout; 
      timeout_type } = RHelper.Timeout_event.next raft_state now in
    if timeout <= 0.0
    then Lwt.return (Timeout timeout_type) 
    else  
      Lwt_unix.sleep timeout 
      >|= (fun () -> Timeout timeout_type)

  let reset_next_timeout threads raft_state = 
    Lwt.cancel threads.next_timeout_t;
    {threads with next_timeout_t = next_timeout raft_state}

end 

let get_client_ipc_f stats configuration id = 
  let (
    req_stream, 
    send_client_response 
  ) = Client_ipc.make configuration stats id in 

  let next_client_request = 
    let f = Rate_limiter.wrap 5000 req_stream in
    fun () -> 
      f () 
      >|=(function
        | [] -> Event.Failure "Client IPC"
        | l  -> Event.Client_request l 
      )  
  in 

  let send_client_responses = fun client_responses -> 
    List.iter (fun response -> 
      send_client_response (Some response)
    ) client_responses 
  in

  (next_client_request, send_client_responses)

let set_server_role state stats = 
  let role = match state.RTypes.role with
    | RTypes.Follower _ -> Server_stats.Follower  
    | RTypes.Candidate _ -> Server_stats.Candidate
    | RTypes.Leader _ -> Server_stats.Leader
  in 
  (Server_stats.set_server_role stats role:unit) 
  
let get_app_ipc_f stats configuration server_id = 
  let (
    send_app_request, 
    response_stream
  ) = App_ipc.make configuration server_id stats in 

  let next_app_response () = 
    Lwt_stream.get response_stream
    >|=(function
      | None -> Event.Failure "App IPC"
      | Some r -> Event.App_response r 
    )
  in

  let send_app_requests = fun app_requests -> 
    List.iter (fun request -> 
      send_app_request (Some request)
    ) app_requests
  in 
  (send_app_requests, next_app_response)

let init_data_from_log_records configuration id = 
  Log_record.make configuration id 
  >>=(fun log_record_handle -> 
    let raft_configuration = configuration.Conf.raft_configuration in 
    let b =  
      RLog.Builder.make raft_configuration.RTypes.max_log_size 
    in
    let f acc log_entry is_committed = 
      let b, commit_index = acc in 
      let b = RLog.Builder.add_log_entry b log_entry in 
      let commit_index = 
        if is_committed && log_entry.RLog.index > commit_index
        then  log_entry.RLog.index
        else commit_index 
      in 
      (b, commit_index)
    in  
    let {RLog.lower_bound; _} = raft_configuration.RTypes.max_log_size in 
    Log_record.read_log_records lower_bound log_record_handle f (b, 0) 
    >|= (fun (b, commit_index) -> 
      let log = RLog.Builder.to_log b in 
      
      let _, current_term = 
        RLog.last_log_index_and_term log
      in 

      (log_record_handle, log, commit_index, current_term)
    )  
  ) 

let get_next_raft_message raft_ipc = 
  Raft_srv_raftipc.get_next raft_ipc 
  >|= (function
      | Raft_srv_raftipc.Failure        -> Event.Failure "Raft IPC"
      | Raft_srv_raftipc.Raft_message x -> Event.Raft_message x
  ) 

let run_server configuration id print_header =
  let stats = 
    let print_header = if print_header then Some () else None in
    Server_stats.make 
      ?print_header 
      ~initial_log_size:0
      ~id ()
  in
  
  let (
    next_client_request, 
    send_client_responses
  ) = get_client_ipc_f stats configuration id in 

  let (
    send_app_requests, 
    next_app_response
  ) = get_app_ipc_f stats configuration id in 

  let raft_ipc = Raft_srv_raftipc.make configuration id in 
  
  let rec server_loop threads state =

    Lwt.nchoose (Event.list_of_threads threads) 

    >>=(fun events -> 

      let now = get_now () in
      
      Server_stats.set_log_count 
          stats 
          (RLog.last_log_index state.Raft_logic.raft_state.RTypes.log);
      set_server_role state.Raft_logic.raft_state stats;

      let process_raft_ipc_result (state, raft_messages, 
                                        client_responses, app_requests) = 
        send_client_responses client_responses;
        send_app_requests app_requests; 
        Raft_srv_raftipc.send ~stats raft_ipc raft_messages; 
        state
      in 

      Lwt_list.fold_left_s (fun (state, threads) event -> 
        match event with
        | Event.Raft_message msg -> (
          Raft_logic.handle_raft_message ~stats ~now state msg
          >|= process_raft_ipc_result
          >|= (fun state ->
            let threads = { threads with 
              Event.next_raft_message_t = get_next_raft_message raft_ipc; 
            } in
            (state, threads)
          )
        )

        | Event.Timeout timeout_type -> (
          Raft_logic.handle_timeout ~stats ~now state timeout_type
          >|= process_raft_ipc_result 
          >|= (fun state -> (state, threads))
        )

        | Event.Failure context -> (
          Printf.eprintf "Exiting, context: %s\n" context; 
          exit 1
        )

        | Event.Client_request client_requests -> (
          Raft_logic.handle_client_requests ~stats ~now state client_requests
          >|= process_raft_ipc_result 
          >|= (fun state -> 
            let threads = {threads with
              Event.next_client_request_t = next_client_request  ();
            } in
            (state, threads)
          )
        )

        | Event.App_response app_response -> (
          Raft_logic.handle_app_response ~stats ~now state app_response
          >|= process_raft_ipc_result 
          >|= (fun state -> 
            let threads = { threads with
              Event.next_app_reponse_t = next_app_response ();
            } in
            (state, threads)
          )
        ) 

      ) (state, threads) events 
      >>= (fun (state, threads) -> 
        (* reset the next time out event since it either time has 
         * elapsed and or the previous time out has happened. *)
        let {Raft_logic.raft_state; _} = state in 
        let threads = Event.reset_next_timeout threads raft_state in 
        server_loop threads state
      )
    )
  in

  let initial_raft_state = RRole.Follower.create
    ~configuration:configuration.Conf.raft_configuration 
    ~now:(get_now ()) 
    ~server_id:id () 
  in

  init_data_from_log_records configuration id
  >>= (fun (log_record_handle, log, commit_index, current_term) ->
      
    let initial_raft_state = {initial_raft_state with 
      RTypes.log; 
      RTypes.commit_index; 
      current_term
    } in 
    
    log_f ~level:Notice ~section 
        "Log read done, commit index: %i, current term: %i" 
        commit_index current_term
    >|= (fun () -> (initial_raft_state, log_record_handle))
  )
  >>= (fun (initial_raft_state, log_record_handle) ->


    let connection_state, app_requests = 
      Raft_logic.initialize configuration id 
    in 

    send_app_requests app_requests;
    let state = Raft_logic.({
      raft_state = initial_raft_state; 
      connection_state;
      log_record_handle;
    }) in
    
    let initial_threads = Event.({
      next_client_request_t = next_client_request ();
      next_raft_message_t = get_next_raft_message raft_ipc;
      next_timeout_t = Event.next_timeout initial_raft_state; 
      next_app_reponse_t  = next_app_response ();
    }) in 

    server_loop initial_threads state
  )

let run configuration id print_header log = 
  let logger_t = 
    if log
    then 
      let basename = Printf.sprintf "raft_server_%07i" id in 
      Raft_utl_logger.start ~basename ~interval:120 () 
    else begin 
      Lwt_log_core.default := Lwt_log_core.null; 
      Lwt.return_unit
    end 
  in 
  let server_t = run_server configuration id print_header in 
  Lwt_main.run @@ Lwt.join [logger_t; server_t] 

let () =
  Random.self_init ();
  

  let id   = ref (-1) in
  let id_spec = Arg.Set_int id in

  let print_header = ref false in 
  let print_header_spec = Arg.Set print_header in
  
  let log = ref false in 
  let log_spec = Arg.Set log  in

  let env, env_spec = Conf.env_arg in

  Arg.parse [
    ("--env", env_spec , " : which env");
    ("--id", id_spec , " : server raft id");
    ("--print-header", print_header_spec, " : enable header printing");
    ("--log", log_spec, " : enable logging");
  ] (fun _ -> ()) "test.ml";

  let configuration = Conf.default_configuration !env in

  let nb_of_servers = 
    List.length configuration.Conf.servers_ipc_configuration 
  in 
  if !id < 0 || !id >= nb_of_servers 
  then begin 
    Printf.eprintf "Invalid server id: %i\n" !id; 
  end;

  run configuration !id !print_header !log
