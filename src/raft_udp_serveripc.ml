open Lwt_log_core
open Lwt.Infix 

module Pb           = Raft_udp_pb
module Server_stats = Raft_udp_serverstats
module Counter      = Raft_udp_counter
module Client_ipc   = Raft_udp_clientipc 
module Log          = Raft_udp_log 
module Log_record   = Raft_udp_logrecord

module RState       = Raft_state

module RPb = Raft_pb 

type raft_message    = RPb.message * int 

type raft_messages   = raft_message list 

type client_request  = Raft_udp_pb.client_request * Raft_udp_clientipc.handle

type client_response = Raft_udp_pb.client_response * Raft_udp_clientipc.handle 

type client_responses = client_response list 

type notifications = RPb.notification list
  
module StringMap = Map.Make(struct
  type t = string
  let compare (x:string) (y:string) = Pervasives.compare x y
end)

module Pending_requests = struct 

  type t = Client_ipc.handle StringMap.t
  
  let add t request_id handle  = 
    StringMap.add request_id handle t
  
  let get t request_id =
    match StringMap.find request_id t with
    | handle ->
      let t = StringMap.remove request_id t  in
      (t, Some handle)
    | exception Not_found ->
      (t, None)

  let empty = StringMap.empty 

end (* Pending_requests *)
  
type connection_state = Pending_requests.t  

type state = {
  raft_state: RPb.state; 
  connection_state: connection_state; 
  log_record_handle : Log_record.t;
}

let initialize () = Pending_requests.empty

let handle_notifications logger connection_state compaction_handle notifications = 

  let connection_state, client_responses = List.fold_left (fun acc notification ->

      match notification with
      | RPb.Committed_data {RPb.rev_log_entries} -> 
        let ids = List.map (fun ({RPb.id; _ }:RPb.log_entry) -> id) rev_log_entries in 
        List.fold_left (fun (connection_state, client_responses) id -> 
            let connection_state, handle = Pending_requests.get connection_state id in 
            match handle with
            | None -> 
              (connection_state, client_responses) 
            | Some handle -> 
              (connection_state, ((Pb.Add_log_success, handle)::client_responses))
        ) acc ids 

      | RPb.New_leader _
      | RPb.No_leader  -> 
          (* TODO need to go through all the pending requests. 
           *)
        acc
    ) (connection_state, []) notifications
  in
  Lwt_list.iter_s (function
    | RPb.Committed_data {RPb.rev_log_entries} -> 
      Log_record.append_commited_data logger rev_log_entries compaction_handle 
    | _ -> Lwt.return_unit
  ) notifications
  >|=(fun () -> (connection_state, client_responses))


let handle_raft_message ~logger ~stats ~now state msg = 
  let { raft_state; connection_state; log_record_handle; } = state in 
  log ~logger ~level:Notice "[ServerIPC] Raft Message Received"
  >>=(fun () ->
    Log.print_state logger raft_state
  )
  >>=(fun () -> 
    Log.print_msg_received logger msg raft_state.RPb.id
  )
  >>=(fun () ->
    Server_stats.tick_raft_msg_recv stats;
    begin match msg with
    | RPb.Append_entries_response {RPb.result = RPb.Log_failure  _ ; _}
    | RPb.Append_entries_response {RPb.result = RPb.Term_failure ; _} ->
      Server_stats.tick_append_entries_failure stats; 
    | _ -> ()
    end;

    let raft_state, responses, notifications = Counter.Perf.f3 (Server_stats.msg_processing stats) 
      Raft_logic.handle_message raft_state msg now
    in

    handle_notifications logger  connection_state log_record_handle notifications 
    >|=(fun (connection_state, client_responses) ->

      ({state with raft_state; connection_state} , responses, client_responses)
    )
  )

let handle_timeout ~logger ~stats ~now state timeout_type = 
  let { raft_state; connection_state; log_record_handle ; } = state in 
  begin match timeout_type with
  | RPb.Heartbeat -> (
    Server_stats.tick_heartbeat stats;
    log ~logger ~level:Notice "[ServerIPC] Heartbeat timeout" 
    >|= (fun () ->
      Counter.Perf.f2 (Server_stats.hb_processing stats)
        Raft_logic.handle_heartbeat_timeout raft_state now
    )
    >|= (fun (raft_state, msgs) -> (raft_state, msgs, []))
  )

  | RPb.New_leader_election -> (
    print_endline "NEW LEADER ELECTION%!";
    log ~logger ~level:Notice "[ServerIPC] Leader Election timeout"
    >|= (fun () ->
      Raft_logic.handle_new_election_timeout raft_state now
    )
  )
  end

  >>=(fun (raft_state, msgs, notifications) ->

    handle_notifications logger connection_state log_record_handle notifications
    >|=(fun (connection_state, client_responses) ->
      ({state with raft_state; connection_state}, msgs, client_responses)
    )
  ) 

let handle_client_request ~logger ~stats ~now  state (client_request, handle) = 

  let { raft_state; connection_state; _ } = state in 

  match client_request with
  | Pb.Ping {Pb.request_id; } -> 
    let client_response = Pb.(Pong {
      request_id; 
      leader_id = RState.current_leader raft_state;
    }) in 

    let client_response = (client_response, handle) in 
    Lwt.return (state, [], [client_response])

  | Pb.Add_log {Pb.request_id; data;} -> 

    let new_log_response  = 
      let datas = [(data, request_id)] in 
      Raft_logic.handle_add_log_entries raft_state datas now 
    in 

    begin match new_log_response with
    | Raft_logic.Delay
    | Raft_logic.Forward_to_leader _ -> 
      log ~logger ~level:Notice "[ServerIPC] Log Rejected "
      >|= (fun () ->

        let client_response = Pb.(Add_log_not_a_leader {
          leader_id = RState.current_leader raft_state;
        }) in 

        let client_response = (client_response, handle) in 
        (state, [], [client_response])
      )

    | Raft_logic.Appended (raft_state, msgs) -> 
      log_f ~logger ~level:Notice "[ServerIPC] Log Added (log size: %i)" raft_state.RPb.log_size 
      >|= (fun () ->
        let connection_state = Pending_requests.add connection_state request_id handle in 
        ({state with raft_state; connection_state} , msgs, [])
      )
    end
