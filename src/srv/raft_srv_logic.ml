open Lwt.Infix 
open !Lwt_log_core

module Counter      = Raft_utl_counter

module APb          = Raft_com_pb
module Server_stats = Raft_srv_serverstats
module Log          = Raft_srv_log 
module Log_record   = Raft_srv_logrecord
module Conf         = Raft_com_conf

module RTypes = Raft_types
module RLog   = Raft_log
module RPb    = Raft_pb 
module RConv  = Raft_pb_conv

type client_request   = Raft_com_pb.client_request * Raft_srv_clientipc.handle
type client_response  = Raft_com_pb.client_response * Raft_srv_clientipc.handle 
type client_responses = client_response list 
type app_requests = Raft_com_pb.app_request list 
type app_response = Raft_com_pb.app_response 

let section = Section.make (Printf.sprintf "%10s" "Logic")

module StringMap = Map.Make(struct
  type t = string
  let compare (x:string) (y:string) = Pervasives.compare x y
end)

module Pending_requests = struct 

  type t = client_request StringMap.t
  
  let add t request_id client_request = 
    StringMap.add request_id client_request t
  
  let get_and_remove t request_id =
    match StringMap.find request_id t with
    | client_request ->
      let t = StringMap.remove request_id t  in
      (t, Some client_request)
    | exception Not_found ->
      (t, None)

  let empty = StringMap.empty 

end (* Pending_requests *)

(* All the outgoing RAFT messages are sent in a dedicated
 * concurrent thread [state.outgoing_message_processing]. The 
 * RAFT protocol nevers requires the sender of a RAFT message to block and 
 * wait for any responses. (In fact that response might never come).
 *
 * In order to send those message concurrently, an [Lwt_stream] is used to 
 * decouple the threads which wants to compute the outgoing messages from 
 * the one which actually sends them.  
 * 
 * For the caller of this API sending a response is simply pushing the 
 * response to the stream (immediate). The Lwt scheduler will then 
 * pick it up asynchronously by scheduling 
 * [state.outgoing_message_processing] *)
  
type connection_state = {
  pending_requests : Pending_requests.t;
    (* Keeps track of pending request from client *)
  raft_ipc : Raft_srv_raftipc.t; 
    (* Communication system for RAFT messages *)
  last_app_index : int; 
    (* The last replicated log index on the app server *)
  app_pending_request : bool; 
    (* [true] when there is a pending request to the app server *)
}

let initialize configuration server_id = 
  let raft_ipc = Raft_srv_raftipc.make configuration server_id in
  let app_requests = [APb.Init] in 
  let connection_state = {
    pending_requests = Pending_requests.empty; 
    raft_ipc;
    last_app_index = -1; 
    app_pending_request = true;
  } in
  (connection_state, app_requests)
    
type state = {
  raft_state: RTypes.state; 
  connection_state: connection_state; 
  log_record_handle : Log_record.t;
}

let get_next_raft_message state = 
  let {connection_state = {raft_ipc; _}; _} = state in 
  Raft_srv_raftipc.get_next raft_ipc

type result = (state * client_responses * app_requests) 

let handle_deleted_logs log_record_handle deleted_logs () = 
  match deleted_logs with
  | [] -> Lwt.return_unit
  | _ ->  Log_record.delete_logs deleted_logs log_record_handle

let handle_added_logs log_record_handle added_logs () = 
  match added_logs with
  | [] -> Lwt.return_unit
  | _ ->
    (* The RAFT protocol dictates that all committed log entries must be stored
     * permanently on DISK. This way, if a server crashes it can recover.  *)
    Log_record.add_logs added_logs log_record_handle 

(* Determine whether a new app_request should be sent. The following 
 * 2 conditions should be met for a new app_request to be sent:
 * - there are no app_request currently pending 
 * - there are committed log entries which indices are past the
 *   last log index returned by the app server.  *)
let make_app_request connection_state raft_state = 
  let {last_app_index; app_pending_request; _} = connection_state in  
  if app_pending_request
  then None 
  else 
    let log_entries = 
      let since = last_app_index in 
      Raft_logic.committed_entries_since ~since raft_state
    in 
    match log_entries with
    | [] -> None 
    | _ -> 
      let log_entries = List.map RConv.log_entry_to_pb log_entries in 
      let request = APb.(Add_log_entries {log_entries}) in
      let connection_state = {connection_state with
        app_pending_request = true; 
      } in 
      Some (connection_state, request) 

let handle_committed_logs state committed_logs () = 
  let {connection_state; log_record_handle; raft_state; _} = state in 
  match committed_logs with
  | [] -> Lwt.return None
  | _ ->
    Log_record.set_committed committed_logs log_record_handle 
    >|=(fun () -> make_app_request connection_state raft_state)

let process_result stats state result =  
  let {log_record_handle; connection_state; _ } = state in 
  let {raft_ipc; _} = connection_state in 

  let {
    Raft_logic.state = raft_state; 
    messages_to_send = outgoing_messages; 
    committed_logs;
    leader_change = _;  
      (* TODO handle leader_change by replying to cleaning up the client
       * pending requests *)
    added_logs; 
    deleted_logs; 
  }  = result in 

  handle_deleted_logs log_record_handle deleted_logs () 
  >>= handle_added_logs log_record_handle added_logs 
  >>= handle_committed_logs state committed_logs
  >|=(fun app_request_res -> 
    Raft_srv_raftipc.send ~stats raft_ipc outgoing_messages;
    match app_request_res with
    | None -> 
      ({state with raft_state; }, [] , [])
    | Some (connection_state, app_request) -> 
      ({state with raft_state; connection_state}, [] , [app_request])
  )

let handle_raft_message ~stats ~now state msg = 
  let {raft_state; _ } = state in 

  Log.print_msg_received section msg 
  >>=(fun () ->
    Server_stats.tick_raft_msg_recv stats;

    let msg = RConv.message_of_pb msg in
    let perf = Server_stats.msg_processing stats in 
    let result = 
      Counter.Perf.f3 perf Raft_logic.handle_message raft_state msg now 
    in

    process_result stats state result  
  )
  >>=(fun (({raft_state; _}, _, _) as r) -> 
    Log.print_state section raft_state
    >|=(fun () -> r)
  )

let handle_timeout ~stats ~now state timeout_type = 
  let { raft_state; _} = state in 
  begin match timeout_type with
    | RTypes.Heartbeat -> (
      Server_stats.tick_heartbeat stats;
      log ~section ~level:Notice "Heartbeat timeout" 
      >|= (fun () ->
        Counter.Perf.f2 (Server_stats.hb_processing stats)
          Raft_logic.handle_heartbeat_timeout raft_state now
      )
    )

    | RTypes.New_leader_election -> (
      Printf.printf "NEW LEADER ELECTION [%2i] \n%!" 
            raft_state.RTypes.server_id;
      log ~level:Notice ~section "Leader Election timeout"
      >|= (fun () ->
        Raft_logic.handle_new_election_timeout raft_state now
      ))
  end
  >>=(fun result -> process_result stats state result) 
  >>=(fun (({raft_state; _}, _, _) as r) -> 
    Log.print_state section raft_state
    >|=(fun () -> r)
  )

let handle_client_requests ~stats ~now  state client_requests = 
  let _ = stats and _ = now in 
  let {connection_state; raft_state; _ } = state in 
  let {pending_requests; _ } = connection_state in 

  let pending_requests, datas = List.fold_left (fun acc client_request ->
    let (pending_requests, datas) = acc in 
    let (client_request_msg, _ (*handle*)) = client_request in 
    match client_request_msg with
    | APb.Add_log_entry {APb.client_log_id; client_log_data}  -> 
      let pending_requests = 
        Pending_requests.add pending_requests client_log_id client_request 
      in 
      (pending_requests, (client_log_data, client_log_id) :: datas) 
  ) (pending_requests, []) client_requests in

  let new_log_response  = 
    Raft_logic.handle_add_log_entries raft_state datas now 
  in 

  begin match new_log_response with
  | Raft_logic.Delay
  | Raft_logic.Forward_to_leader _ -> 
    log ~level:Notice ~section "New logs rejected since not a leader"
    >|= (fun () ->

      let client_responses = List.map (fun (_, handle) -> 
        let client_response_msg = APb.(Add_log_not_a_leader {
          leader_id = RTypes.current_leader raft_state
        }) in 

        (client_response_msg, handle)
      ) client_requests in

      (state, client_responses, [])
    )

  | Raft_logic.Appended result -> 
    let log_size = RLog.last_log_index raft_state.RTypes.log in
    log_f ~level:Notice ~section 
          "Log Added (log size: %i) (nb logs: %i)" 
          log_size (List.length datas)  
    >>= (fun () ->
      let state = {state with 
        connection_state = {connection_state with pending_requests; }
      } in 
      process_result stats state result  
    )
  end
      
let process_app_result is_leader acc result =  
  let (pending_requests, client_responses)  = acc in 

  let {APb.index = _; id; APb.result_data} = result  in 
  
  let (
    pending_requests, 
    client_request
  ) = Pending_requests.get_and_remove pending_requests id in 

  match client_request with
  | None -> 
    if is_leader
    then 
      log_f ~level:Error ~section 
            "Could not find pending request after validation for id: %s" id 
      >|=(fun () -> (pending_requests, client_responses)) 
    else 
      Lwt.return (pending_requests, client_responses) 

  | Some (APb.Add_log_entry _, handle) ->
    let client_response_msg = APb.Add_log_result {
      APb.client_log_id = id; 
      APb.client_log_result_data = result_data; 
    } in 
    let client_responses = (client_response_msg, handle)::client_responses in
    Lwt.return (pending_requests, client_responses) 

let handle_app_response ~stats ~now state app_response = 
  let _ = stats and _ = now in 

  let {connection_state; raft_state; _ } = state in 
  let {pending_requests; _} = connection_state in 

  match app_response with
  | APb.Add_log_results {APb.results; last_log_index} -> 

    Lwt_list.fold_left_s 
        (process_app_result (RTypes.is_leader raft_state)) 
        (pending_requests, []) 
        results 
    >|=(fun (pending_requests, client_responses) ->
      
      let connection_state = {connection_state with 
        pending_requests; 
        app_pending_request = false; 
        last_app_index = last_log_index;
      } in 

      match make_app_request connection_state raft_state with
      | None -> 
        ({state with connection_state}, client_responses, []) 
      | Some (connection_state, app_request) -> 
        ({state with connection_state}, client_responses, [app_request]) 
    )
