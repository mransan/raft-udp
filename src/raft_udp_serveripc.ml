open Lwt_log_core
open Lwt.Infix 

module Pb           = Raft_udp_pb
module Server_stats = Raft_udp_serverstats
module Counter      = Raft_udp_counter
module Client_ipc   = Raft_udp_clientipc 
module Log          = Raft_udp_log 

type raft_message    = Raft_pb.message * int 

type raft_messages   = raft_message list 

type client_request  = Raft_udp_pb.client_request * Raft_udp_clientipc.handle

type client_response = Raft_udp_pb.client_response * Raft_udp_clientipc.handle 

type client_responses = client_response list 

type notifications = Raft_pb.notification list
  
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

type state = Raft_pb.state * connection_state 

let initialize () = Pending_requests.empty

let handle_notifications connection_state notifications = 

  List.fold_left (fun acc notification ->

    match notification with
    | Raft_pb.Committed_data {Raft_pb.ids} -> 
      List.fold_left (fun (connection_state, client_responses) id -> 
          let connection_state, handle = Pending_requests.get connection_state id in 
          match handle with
          | None -> 
            (connection_state, client_responses) 
          | Some handle -> 
            (connection_state, ((Pb.Add_log_success, handle)::client_responses))
      ) acc ids 

    | Raft_pb.New_leader _
    | Raft_pb.No_leader  -> 
        (* TODO need to go through all the pending requests. 
         *)
      acc
  ) (connection_state, []) notifications


let handle_raft_message ~logger ~stats ~now (raft_state, connection_state) msg = 
  log ~logger ~level:Notice "[ServerIPC] Raft Message Received"
  >>=(fun () ->
    Log.print_state logger raft_state
  )
  >>=(fun () -> 
    Log.print_msg_received logger msg raft_state.Raft_pb.id
  )
  >|=(fun () ->
    Server_stats.tick_raft_msg_recv stats;
    begin match msg with
    | Raft_pb.Append_entries_response {Raft_pb.result = Raft_pb.Log_failure  _ ; _}
    | Raft_pb.Append_entries_response {Raft_pb.result = Raft_pb.Term_failure ; _} ->
      Server_stats.tick_append_entries_failure stats; 
    | _ -> ()
    end;

    let raft_state, responses, notifications = Counter.Perf.f3 (Server_stats.msg_processing stats) 
      Raft_logic.handle_message raft_state msg now
    in

    let connection_state, client_responses = 
      handle_notifications connection_state notifications 
    in 
    (* TODO: handle notifications *)

    ((raft_state, connection_state) , responses, client_responses)
  )

let handle_timeout ~logger ~stats ~now (raft_state, connection_state) timeout_type = 
  begin match timeout_type with
  | Raft_pb.Heartbeat -> (
    Server_stats.tick_heartbeat stats;
    log ~logger ~level:Notice "[ServerIPC] Heartbeat timeout" 
    >|= (fun () ->
      Counter.Perf.f2 (Server_stats.hb_processing stats)
        Raft_logic.handle_heartbeat_timeout raft_state now
    )
    >|= (fun (raft_state, msgs) -> (raft_state, msgs, []))
  )

  | Raft_pb.New_leader_election -> (
    print_endline "NEW LEADER ELECTION%!";
    log ~logger ~level:Notice "[ServerIPC] Leader Election timeout"
    >|= (fun () ->
      Raft_logic.handle_new_election_timeout raft_state now
    )
  )
  end

  >|=(fun (raft_state, msgs, notifications) ->

    let connection_state, client_responses = 
      handle_notifications connection_state notifications
    in 
    ((raft_state, connection_state), msgs, client_responses)
  ) 

let handle_client_request ~logger ~stats ~now (raft_state, connection_state) (client_request, handle) = 

  match client_request with
  | Pb.Ping {Pb.request_id; } -> 
    let client_response = Pb.(Pong {
      request_id; 
      leader_id = Raft_helper.State.current_leader raft_state;
    }) in 

    let client_response = (client_response, handle) in 
    Lwt.return ((raft_state, connection_state), [], [client_response])

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
          leader_id = Raft_helper.State.current_leader raft_state;
        }) in 

        let client_response = (client_response, handle) in 
        ((raft_state, connection_state), [], [client_response])
      )

    | Raft_logic.Appended (raft_state, msgs) -> 
      log_f ~logger ~level:Notice "[ServerIPC] Log Added (log size: %i)" raft_state.Raft_pb.log_size 
      >|= (fun () ->
        let connection_state = Pending_requests.add connection_state request_id handle in 
        ((raft_state, connection_state) , msgs, [])
      )
    end
