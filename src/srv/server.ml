open Lwt.Infix
open !Lwt_log_core

module RPb = Raft_pb
module RHelper = Raft_helper
module RLog = Raft_log
module RTypes = Raft_types
module RConv = Raft_pb_conv

module APb = Raft_com_pb
module Conf = Raft_com_conf
module Server_stats = Raft_srv_stats
module Raft_ipc = Raft_srv_raftipc
module Client_ipc = Raft_srv_clientipc
module App_ipc = Raft_srv_appipc
module Storage = Raft_srv_storage
module Rate_limiter = Raft_utl_ratelimiter
module Debug = Raft_com_debug

let section = Section.make (Printf.sprintf "%10s" "server")

module Stats = struct  
  module Counter = Raft_utl_counter.Counter 

  let set_server_role {Raft_srv_logic.raft_state = {RTypes.role; _}; _} = 
    let role = match role with
      | RTypes.Follower _ -> Server_stats.Follower  
      | RTypes.Candidate _ -> Server_stats.Candidate
      | RTypes.Leader _ -> Server_stats.Leader
    in 
    Server_stats.server_role := (Some role)
  
  let set_log {Raft_srv_logic.raft_state = {RTypes.log; _} ; _} = 
    Counter.set Server_stats.log (RLog.last_log_index log) 
end 
  
let get_now =
  (* Mtime guarantees monotomic time which is a requirement from 
   * the RAFT library *)
  let t0 = Mtime.counter () in 
  (fun () -> Mtime.(count t0 |> to_s)) 

module Event = struct 
  type e  = [ 
    | `Failure        of string  
      (** Fatal failure, the server will exit *)
    | `Raft_message   of Raft_ipc.message 
      (** A RAFT message was received from one of the other RAFT servers *)
    | `Client_request of Client_ipc.client_request list
      (** A Client request was received *)
    | `App_response   of App_ipc.app_response 
      (** A response was received from the App server *)
    | `Timeout        of Raft_types.timeout_type
      (** RAFT Protocol timeout happened *)
  ]

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
      timeout_type } = RHelper.Timeout_event.next ~now raft_state in
    if timeout <= 0.0
    then 
      Lwt.return (`Timeout timeout_type) 
    else  
      Lwt_unix.sleep timeout 
      >|= (fun () -> `Timeout timeout_type)

  let reset_next_timeout threads raft_state = 
    Lwt.cancel threads.next_timeout_t;
    {threads with next_timeout_t = next_timeout raft_state}
end 

let get_next_raft_message raft_ipc =
  (Raft_ipc.get_next raft_ipc :> Event.e Lwt.t) 

let get_next_client_requests client_ipc = 
  (Client_ipc.get_next client_ipc :> Event.e Lwt.t)

let get_next_app_response app_ipc = 
  (App_ipc.get_next app_ipc :> Event.e Lwt.t) 

let init_server configuration id print_header = 
  Server_stats.server_id := id;
  Server_stats.print_header := print_header;

  (* Initialize the 3 IPC system of the RAFT server *)

  let client_ipc = Client_ipc.make configuration id in
  let app_ipc = App_ipc.make configuration id in 
  let raft_ipc = Raft_ipc.make configuration id in 
  Storage.make configuration id
  >>=(fun storage -> 
    Storage.read_raft_state ~now:(get_now ()) storage
    >>=(fun raft_state -> 
      log ~level:Notice ~section "Read from persistent storage done" 
      >>=(fun () -> Debug.print_state section raft_state) 
      >|=(fun () -> (storage, raft_state))
    )
  ) 
  >|= (fun (storage, raft_state) ->

    let connection_state, app_requests = 
      Raft_srv_logic.initialize configuration id 
    in 

    App_ipc.send app_ipc app_requests;

    let state = Raft_srv_logic.({
      raft_state;
      connection_state;
      storage;
    }) in
    
    let threads = Event.({
      next_client_request_t = get_next_client_requests client_ipc; 
      next_raft_message_t = get_next_raft_message raft_ipc;
      next_app_reponse_t  = get_next_app_response app_ipc;
      next_timeout_t = Event.next_timeout raft_state; 
    }) in 

    (client_ipc, app_ipc, raft_ipc, threads, state) 
  )

let run_server (client_ipc, app_ipc, raft_ipc, threads, state) =
      
  let process_raft_ipc_result (state, raft_messages, 
                               client_responses, app_requests) = 
    Client_ipc.send client_ipc client_responses;
    App_ipc.send app_ipc app_requests; 
    Raft_ipc.send raft_ipc raft_messages; 
    state
  in 
  
  let rec server_loop threads state =

    Lwt.nchoose (Event.list_of_threads threads) 

    >>=(fun events -> 
      
      Stats.set_log state; 
      Stats.set_server_role state;

      Lwt_list.fold_left_s (fun (state, threads) event -> 
        let now = get_now () in

        match event with
        | `Raft_message msg -> (
          Raft_srv_logic.handle_raft_message ~now state msg
          >|= process_raft_ipc_result
          >|= (fun state ->
            let threads = { threads with 
              Event.next_raft_message_t = get_next_raft_message raft_ipc; 
            } in
            (state, threads)
          )
        )

        | `Timeout timeout_type -> (
          Raft_srv_logic.handle_timeout ~now state timeout_type
          >|= process_raft_ipc_result 
          >|= (fun state -> (state, threads))
        )

        | `Failure context -> (
          Printf.eprintf "Exiting, context: %s\n" context; 
          exit 1
        )

        | `Client_request client_requests -> (
          Raft_srv_logic.handle_client_requests ~now state client_requests
          >|= process_raft_ipc_result 
          >|= (fun state -> 
            let threads = {threads with
              Event.next_client_request_t = get_next_client_requests client_ipc;
            } in
            (state, threads)
          )
        )

        | `App_response app_response -> (
          Raft_srv_logic.handle_app_response ~now state app_response
          >|= process_raft_ipc_result 
          >|= (fun state -> 
            let threads = { threads with
              Event.next_app_reponse_t = get_next_app_response app_ipc;
            } in
            (state, threads)
          )
        ) 

      ) (state, threads) events 
      >>= (fun (state, threads) -> 
        (* reset the next time out event since it either time has 
         * elapsed and or the previous time out has happened. *)
        let {Raft_srv_logic.raft_state; _} = state in 
        let threads = Event.reset_next_timeout threads raft_state in 
        server_loop threads state
      )
    )
  in
  server_loop threads state 

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
  let server_t = 
    init_server configuration id print_header 
    >>= run_server 
  in
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
