open Lwt_log_core
open Lwt.Infix 

module Udp   = Raft_udp_pb
module Raft  = Raft_pb
module Stats = Raft_udp_serverstats
module Perf  = Raft_udp_counter.Perf 

type raft_message    = Raft_pb.message * int 

type raft_messages   = raft_message list 

type client_request  = Raft_udp_pb.client_request * Raft_udp_clientipc.handle

type client_response = Raft_udp_pb.client_response * Raft_udp_clientipc.handle 

type client_responses = client_response list 

let handle_raft_message ~logger ~stats ~now raft_state msg = 
  log ~logger ~level:Notice ">> Raft Message Received"
  >>=(fun () ->
    Raft_udp_log.print_state logger raft_state
  )
  >>=(fun () ->
    Stats.tick_raft_msg_recv stats;
    begin match msg with
    | Raft.Append_entries_response {Raft.result = Raft.Log_failure  _ ; _}
    | Raft.Append_entries_response {Raft.result = Raft.Term_failure ; _} ->
      Stats.tick_append_entries_failure stats; 
    | _ -> ()
    end;

    let raft_state, responses, notifications = Perf.f3 (Stats.msg_processing stats) 
      Raft_logic.handle_message raft_state msg now
    in
    (* 
     * TODO Handle notifications 
     *)
    Raft_udp_log.print_msg_received logger msg raft_state.Raft.id
    >|=(fun () -> (raft_state, responses, []))
  )

let handle_timeout ~logger ~stats ~now raft_state timeout_type = 
  begin match timeout_type with
  | Raft.Heartbeat -> (
    Stats.tick_heartbeat stats;
    log ~logger ~level:Notice ">> Heartbeat timeout" 
    >|= (fun () ->
      Perf.f2 (Stats.hb_processing stats)
        Raft_logic.handle_heartbeat_timeout raft_state now
    )
    >|= (fun (raft_state, msgs) -> (raft_state, msgs, []))
  )

  | Raft.New_leader_election -> (
    print_endline "NEW LEADER ELECTION%!";
    log ~logger ~level:Notice ">> Leader Election timeout"
    >|= (fun () ->
      Raft_logic.handle_new_election_timeout raft_state now
    )
  )
  end

  >|=(fun (raft_state, msgs, notifications) ->
    (* 
     * TODO Handle notifications 
     *)
    (raft_state, msgs, [])
  ) 

let handle_client_request ~logger ~stats ~now raft_state (client_request, handle) =  
  match client_request with
  | Udp.Ping {Udp.request_id; } -> 
    begin  
      let client_response = Udp.(Pong {
        request_id; 
        leader_id = Raft_helper.State.current_leader raft_state; 
      }) in 
      Lwt.return (raft_state, [], [(client_response, handle)])
    end 

  | Udp.Add_log {Udp.request_id; data} -> 
    
    let new_log_response  = 
      let datas = [(data, request_id)] in 
      Raft_logic.handle_add_log_entries raft_state datas now 
    in 

    match new_log_response with
    | Raft_logic.Delay
    | Raft_logic.Forward_to_leader _ -> 
      begin 
        let client_response = Udp.(Add_log_not_a_leader {
          leader_id = Raft_helper.State.current_leader raft_state; 
        }) in 
        Lwt.return (raft_state, [], [(client_response, handle)])
      end

    | Raft_logic.Appended (raft_state, msgs) -> 
      begin 
        log_f ~logger ~level:Notice ">> Log Added (log size: %i)" raft_state.Raft_pb.log_size 
        >|= (fun () ->
          (raft_state, msgs, [(Udp.Add_log_success, handle)])
        )
      end
