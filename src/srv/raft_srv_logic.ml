open Lwt.Infix 
open !Lwt_log_core

module RTypes = Raft_types
module RLog = Raft_log
module RProtocol = Raft_protocol

module RConv = Raft_pb_conv

module Counter = Raft_utl_counter
module APb = Raft_com_pb
module Server_stats = Raft_srv_stats
module Debug = Raft_com_debug
module Storage = Raft_srv_storage

let section = Section.make (Printf.sprintf "%10s" "Logic")

module Stats = struct 
  module Counter = Raft_utl_counter.Counter 

  let tick_heartbeat () =
    Counter.incr Server_stats.heartbeat
end 

type client_request = Raft_com_pb.client_request * Raft_srv_clientipc.handle

type client_response = Raft_com_pb.client_response * Raft_srv_clientipc.handle 

type client_responses = client_response list 

type app_requests = Raft_com_pb.app_request list 

type app_response = Raft_com_pb.app_response 

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
  last_app_index : int; 
    (* The last replicated log index on the app server *)
  app_pending_request : bool; 
    (* [true] when there is a pending request to the app server *)
}

let initialize _ _ = 
  let app_requests = [APb.Init] in 
  let connection_state = {
    pending_requests = Pending_requests.empty; 
    last_app_index = -1; 
    app_pending_request = true;
  } in
  (connection_state, app_requests)
    
type state = {
  raft_state: RTypes.state; 
  connection_state: connection_state; 
  storage : Storage.t;
}

type result = (
  state * 
  RTypes.message_to_send list * 
  client_responses * 
  app_requests
) 

let handle_deleted_logs storage deleted_logs () = 
  match deleted_logs with
  | [] -> Lwt.return_unit
  | _ ->  Storage.delete_logs deleted_logs storage

let handle_added_logs storage added_logs () = 
  match added_logs with
  | [] -> Lwt.return_unit
  | _ ->
    (* The RAFT protocol dictates that all committed log entries must be stored
     * permanently on DISK. This way, if a server crashes it can recover.  *)
    Storage.add_logs added_logs storage 

(* Determine whether a new app_request should be sent. The following 
 * 2 conditions should be met for a new app_request to be sent:
 * - there are no app_request currently pending 
 * - there are committed log entries which indices are past the
 *   last log index returned by the app server.  *)
let make_app_request connection_state raft_state = 
  let {last_app_index; app_pending_request; _} = connection_state in  
  if app_pending_request
  then 
    log ~level:Notice ~section 
        "No new app request due to existing pending request"
    >|=(fun () -> None)
  else 
    let log_entries = 
      let since = last_app_index in 
      RProtocol.committed_entries_since ~since raft_state
    in 
    match log_entries with
    | [] -> 
      log_f ~level:Notice ~section
          "No new app request to no new committed log since: %i"
              last_app_index
      >|=(fun () -> None)
    | _ -> 
      let log_entries = List.map RConv.log_entry_to_pb log_entries in 
      let request = APb.(Add_log_entries {log_entries}) in
      let connection_state = {connection_state with
        app_pending_request = true; 
      } in 
      Lwt.return (Some (connection_state, request)) 

let handle_committed_logs state committed_logs () = 
  let {connection_state; storage; raft_state; _} = state in 
  match committed_logs with
  | [] -> Lwt.return None
  | _ ->
    Storage.set_committed committed_logs storage 
    >>=(fun () -> make_app_request connection_state raft_state) 

(* This function compute the `not a leader` client response for 
 * all the pending requests. It returns the new empty pending
 * requests *)
let handle_leader_change pending_request leader_change = 

  let client_responses = 
    StringMap.bindings pending_request 
    |> List.map (fun (_, (_, handle)) -> 
      let leader_id = match leader_change with
        | RTypes.New_leader i -> Some i 
        | RTypes.No_leader -> None
      in
      let client_response_msg = APb.(Add_log_not_a_leader {leader_id}) in 
      (client_response_msg, handle) 
    )
  in 
  (Pending_requests.empty, client_responses)

let process_result state result =  
  let {storage; connection_state; _ } = state in 

  let {
    RTypes.state = raft_state; 
    messages_to_send = outgoing_messages; 
    committed_logs;
    leader_change;  
      (* TODO handle leader_change by replying to cleaning up the client
       * pending requests *)
    added_logs; 
    deleted_logs; 
  }  = result in 

  let state, client_responses = 
    match leader_change with
    | None -> 
      ({state with raft_state}, []) 

    | Some leader_change -> 
      let {pending_requests; _} = connection_state in 
      let pending_requests, client_responses = 
        handle_leader_change pending_requests leader_change 
      in 
      let connection_state = {connection_state with
        pending_requests; 
      } in 
      ({state with raft_state; connection_state}, client_responses)
  in  

  handle_deleted_logs storage deleted_logs () 
  >>= handle_added_logs storage added_logs 
  >>= handle_committed_logs state committed_logs
  >>=(fun app_request_res -> 

    match app_request_res with
    | None -> 
      Debug.print_state section raft_state
      >|=(fun () -> (state, outgoing_messages, client_responses, []))
    | Some (connection_state, app_request) -> 
      let state = {state with connection_state} in 
      Debug.print_state section raft_state
      >|=(fun () -> (state, outgoing_messages, client_responses, [app_request]))
  )

let handle_raft_message ~now state msg = 
  let {raft_state; _ } = state in 
  let perf = Server_stats.msg_processing in 
  let result = 
    Counter.Perf.f3 perf RProtocol.handle_message raft_state msg now 
  in
  process_result state result  

let handle_timeout ~now state timeout_type = 
  let { raft_state; _} = state in 
  begin match timeout_type with
    | RTypes.Heartbeat -> (
      Stats.tick_heartbeat ();
      log ~section ~level:Notice "Heartbeat timeout" 
      >|= (fun () ->
        Counter.Perf.f2 Server_stats.hb_processing 
          RProtocol.handle_heartbeat_timeout raft_state now
      )
    )

    | RTypes.New_leader_election -> (
      Printf.printf "NEW LEADER ELECTION [%2i] \n%!" 
            raft_state.RTypes.server_id;
      log ~level:Notice ~section "Leader Election timeout"
      >|= (fun () ->
        RProtocol.handle_new_election_timeout raft_state now
      ))
  end
  >>=(fun result -> process_result state result) 

let handle_client_requests ~now  state client_requests = 
  let _ = now in 
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
    Counter.Perf.f3 
      Raft_srv_stats.add_log_processing 
      RProtocol.handle_add_log_entries raft_state datas now 
  in 

  begin match new_log_response with
  | RProtocol.Delay
  | RProtocol.Forward_to_leader _ -> 
    log ~level:Notice ~section "New logs rejected since not a leader"
    >|= (fun () ->

      let client_responses = List.map (fun (_, handle) -> 
        let client_response_msg = APb.(Add_log_not_a_leader {
          leader_id = RTypes.current_leader raft_state
        }) in 

        (client_response_msg, handle)
      ) client_requests in

      (state, [], client_responses, [])
    )

  | RProtocol.Appended result -> 
    let log_size = RLog.last_log_index raft_state.RTypes.log in
    log_f ~level:Notice ~section 
          "Log Added (log size: %i) (nb logs: %i)" 
          log_size (List.length datas)  
    >>= (fun () ->
      let state = {state with 
        connection_state = {connection_state with pending_requests; }
      } in 
      process_result state result  
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

let handle_app_response ~now state app_response = 
  let _ = now in 

  let {connection_state; raft_state; _ } = state in 
  let {pending_requests; _} = connection_state in 

  match app_response with
  | APb.Add_log_results {APb.results; last_log_index} -> 

    Lwt_list.fold_left_s 
        (process_app_result (RTypes.is_leader raft_state)) 
        (pending_requests, []) 
        results 
    >>=(fun (pending_requests, client_responses) ->
      
      let connection_state = {
        pending_requests; 
        app_pending_request = false; 
        last_app_index = last_log_index;
      } in 

      make_app_request connection_state raft_state
      >|=(function 
        | None -> 
          ({state with connection_state}, [], client_responses, []) 
        | Some (connection_state, app_request) -> 
          ({state with connection_state}, [], client_responses, [app_request]) 
      )
    )
