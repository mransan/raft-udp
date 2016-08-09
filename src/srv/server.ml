open Lwt.Infix
open !Lwt_log_core

module RPb = Raft_pb
module RHelper = Raft_helper
module RRole = Raft_role
module RLog = Raft_log
module RState = Raft_state

module Com_pb = Raft_com_pb 
module App_pb = Raft_app_pb
module Conf = Raft_com_conf
module Server_stats = Raft_srv_serverstats
module Raft_ipc = Raft_srv_raftipc
module Raft_ptc = Raft_srv_raftptc
module Client_ipc = Raft_clt_server
module Compaction = Raft_srv_compaction
module Log_record = Raft_srv_logrecord
module App_ipc = Raft_app_client

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
      (** Fatal failure, the server will exit *)

    | Raft_message   of Raft_pb.message 
      (** A RAFT message was received from one of the other RAFT servers *)

    | Client_request of Client_ipc.request list
      (** A Client request was received *)

    | Timeout        of Raft_pb.timeout_event_time_out_type  
      (** RAFT Protocol timeout happened *)

    | Compaction_initiate
      (** Notification that the compaction task should start *)

    | Compaction_update  of RPb.log_interval list 
      (** Notification that the compaction task is done with the list 
          of modified log intervals. 
        *)

    | App_response   of App_pb.app_response 
      (** A response was received from the App server *)

  type threads = {
    next_raft_message_t : e Lwt.t;
    next_client_request_t : e Lwt.t;
    next_timeout_t : e Lwt.t;
    compaction_t : e Lwt.t;
    next_app_reponse_t : e Lwt.t;
  }

  let list_of_threads threads =
    let {
      next_raft_message_t;
      next_client_request_t;
      next_timeout_t;
      compaction_t;
      next_app_reponse_t;
    } = threads in 
    next_raft_message_t   :: 
    next_client_request_t ::
    next_timeout_t        ::
    compaction_t          :: 
    next_app_reponse_t    :: []

end 

let set_server_role state stats = 
  let role = match state.RState.role with
    | RPb.Follower _ -> Server_stats.Follower  
    | RPb.Candidate _ -> Server_stats.Candidate
    | RPb.Leader _ -> Server_stats.Leader
  in 
  (Server_stats.set_server_role stats role:unit) 

let next_timeout timeout timeout_type = 
  Lwt_unix.sleep timeout 
  >|= (fun () -> Event.Timeout timeout_type)

let init_client_ipc logger stats configuration id = 
  let (
    req_stream, 
    send_client_response 
  ) = Client_ipc.make logger configuration stats id in 

  let next_client_request () = 
    Lwt_stream.get req_stream 
     >|= (function 
       | None -> Event.Failure "Client IPC" 
       | Some hd -> 
         let tl = Lwt_stream.get_available req_stream in 
         Event.Client_request (hd::tl)
     )
  in 

  let send_client_responses = fun client_responses -> 
    List.iter (fun client_response -> 
      send_client_response client_response
    ) client_responses 
  in

  (next_client_request, send_client_responses)

let init_app_ipc logger stats configuration id = 
  
  match App_ipc.make logger configuration stats id with
  | None -> 
    failwith (Printf.sprintf "Error starting App Ipc for server id: %i" id)

  | Some (send_app_request, response_stream) ->

    let next_app_response () = 
      Lwt_stream.get response_stream
      >|=(function
        | None  -> Event.Failure "App IPC"
        | Some r-> Event.App_response r 
      )
    in

    let send_app_requests = fun app_requests -> 
      List.iter (fun app_request -> 
        send_app_request app_request
      ) app_requests
    in 
    (send_app_requests, next_app_response)


let init_raft_ipc logger stats id configuration = 

  let raft_ipc = 
    Raft_srv_raftipc.make ~logger ~stats ~server_id:id ~configuration ()  
  in 
  
  let next_raft_message  = fun () -> 
    Raft_srv_raftipc.next_message raft_ipc 
    >|= (function 
      | Ok raft_message -> Event.Raft_message raft_message 
      | Error error_message -> Event.Failure error_message 
    )  
  in 

  let send_raft_messages raft_messages = 
    Raft_ipc.send_messages raft_ipc raft_messages; 
  in 
  (next_raft_message, send_raft_messages) 

let init_compaction configuration = 
  let next_compaction = fun () ->
    Lwt_unix.sleep configuration.Com_pb.disk_backup.Com_pb.compaction_period
    >|=(fun () -> Event.Compaction_initiate)
  in 
  next_compaction 

let run_server configuration id logger print_header slow =

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
  ) = init_client_ipc logger stats configuration id in 
  
  let (
    send_app_requests, 
    next_app_response
  ) = init_app_ipc logger stats configuration id in 

  let (
    next_raft_message, 
    send_raft_messages
  ) = init_raft_ipc logger stats id configuration in 

  let next_compaction = init_compaction configuration in 

  let handle_ptc_result (state, client_responses, app_requests, raft_messages) =  
    send_client_responses client_responses;
    send_app_requests app_requests;
    send_raft_messages raft_messages; 
    state
  in 

  let rec server_loop threads state =

    Lwt.nchoose (Event.list_of_threads threads) 

    >>=(fun events -> 

      let handle_follow_up_action threads state =  
        let now = get_now () in
        let {RPb.timeout; timeout_type } = 
          RHelper.Timeout_event.next (Raft_ptc.raft_state state) now 
        in
        Lwt.cancel threads.Event.next_timeout_t;
        let threads = {threads with 
          Event.next_timeout_t = next_timeout timeout timeout_type
        } in 
        server_loop threads state 
      in
      
      let now = get_now () in

      begin 
        let raft_state = Raft_ptc.raft_state state in 
        Server_stats.set_log_count stats raft_state.RState.log.RLog.log_size;
        set_server_role raft_state stats;
      end; 

      Lwt_list.fold_left_s (fun (state, threads) event -> 
        match event with
        | Event.Raft_message msg -> (
          begin 
            if slow 
            then Lwt_unix.sleep  0.1
            else Lwt.return_unit 
          end
          >>=(fun () -> 
            Raft_ptc.handle_raft_message state now msg 
            >|= handle_ptc_result 
            >|= (fun state ->
              let threads = {threads with 
                Event.next_raft_message_t = next_raft_message (); 
              } in
              (state, threads)
            )
          )
        )

        | Event.Timeout timeout_type -> (
          Raft_ptc.handle_timeout state now timeout_type
          >|= handle_ptc_result 
          >|=(fun state -> (state, threads))
        )

        | Event.Failure context -> (
          Printf.eprintf "Exiting server %i, context: %s\n" id context; 
          exit 1
        )

        | Event.Client_request client_requests -> (
          Raft_ptc.handle_client_requests state now client_requests
          >|= handle_ptc_result 
          >|=(fun state ->
            let threads = { threads with
              Event.next_client_request_t = next_client_request  ();
            } in
            (state, threads)
          )
        )

        | Event.Compaction_initiate -> (
          let raft_state= Raft_ptc.raft_state state in 
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
          Raft_ptc.handle_compaction_update compacted_intervals state
          >|=(fun state ->
            let threads = {threads with Event.compaction_t = next_compaction  ()} in 
            (state, threads) 
          )
        ) 

        | Event.App_response app_response -> (
          Raft_ptc.handle_app_response state now app_response
          >|= handle_ptc_result 
          >|=(fun state ->
            let threads = { threads with
              Event.next_app_reponse_t = next_app_response ();
            } in
            (state, threads)
          )
        ) 

      ) (state, threads) events 
      >>= (fun (state, threads) -> handle_follow_up_action threads state) 
    )
  in

  let initial_raft_state = RRole.Follower.create
    ~configuration:configuration.Com_pb.raft_configuration 
    ~now:(get_now ()) 
    ~id () 
  in

  Compaction.load_previous_log_intervals logger configuration id 
  >|=(fun log_intervals ->
    let builder1 = RLog.Builder.make_t1 () in 
    List.fold_left (fun builder1 log_interval -> 
      RLog.Builder.add_interval builder1 log_interval 
    ) builder1 log_intervals 
  ) 
  >>=(fun builder1 -> 
    let section = Section.make (Printf.sprintf "%10s" "Recovery") in 

    let builder2 = RLog.Builder.t2_of_t1 builder1 in 
    log ~logger ~level:Notice ~section "Global cache done..."
    >>=(fun () ->
      Log_record.read_log_records ~logger configuration id (fun builder2 log_entry ->
        RLog.Builder.add_log_entry builder2 log_entry
      ) builder2 
      >|= RLog.Builder.log_of_t2 
    )
    >>= (fun log -> 
      let initial_raft_state = {initial_raft_state with RState.log } in 
      let commit_index, current_term = RLog.last_log_index_and_term initial_raft_state.RState.log in 
      let initial_raft_state = {initial_raft_state with RState.commit_index; current_term} in 
      
      log_f 
        ~logger 
        ~level:Notice 
        ~section 
        "Log read done, commit index: %i, current term: %i" 
        commit_index current_term
      >|=(fun () -> initial_raft_state)
    )
  )
  >>=(fun initial_raft_state ->
    Log_record.make ~logger configuration id 
    >>=(fun handle ->

      let {
        RPb.timeout; 
        timeout_type
      } = RHelper.Timeout_event.next initial_raft_state (get_now ()) in 

      let initial_threads = Event.({
        next_client_request_t = next_client_request ();
        next_raft_message_t = next_raft_message  ();
        next_timeout_t = next_timeout timeout timeout_type;
        compaction_t = next_compaction ();
        next_app_reponse_t  = next_app_response ();
      }) in 

      let state = Raft_ptc.make
        ~logger 
        ~stats
        ~raft_state:initial_raft_state
        ~log_record_handle:handle
        () 
      in 
      server_loop initial_threads state 
    )
  )

let run configuration id print_header slow log = 
  let t = 
    begin 
      if log
      then 
        let file_name = Printf.sprintf "raft_upd_%i.log" id in 
        let template  = "$(date).$(milliseconds) [$(level)] [$(section)] : $(message)" in
        Lwt_log.file ~mode:`Append ~template ~file_name () 
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

  let id, id_spec = Conf.get_id_cmdline configuration in 

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
