
module Raft = Raft_pb

open Lwt.Infix 
open Lwt_log_core

let string_of_message_type = function
  | Raft.Request_vote_request  _   -> "Request Vote Request" 
  | Raft.Request_vote_response _   -> "Request Vote Response"
  | Raft.Append_entries_request  _ -> "Append Entries Request"
  | Raft.Append_entries_response _ -> "Append Entries Response"

let print_msg_details logger msg () = 
  match msg with
  | Raft.Request_vote_request r -> 
    log_f ~logger ~level:Notice "\t term: %2i - last log: (%2i, %2i)"
      r.Raft.candidate_term 
      r.Raft.candidate_last_log_index
      r.Raft.candidate_last_log_term

  | Raft.Request_vote_response r-> 
    if r.Raft.vote_granted 
    then 
      log_f ~logger ~level:Notice "\t Granted - term: %i"
        r.Raft.voter_term
    else
      log_f ~logger ~level:Notice "\t Rejected - term: %i" 
        r.Raft.voter_term

  | Raft.Append_entries_request r-> 
    begin match r.Raft.rev_log_entries with
    | [] -> 
      log_f ~logger ~level:Notice "\t Heartbeat - prev index: %10i, leader commit: %10i"
        r.Raft.prev_log_index 
        r.Raft.leader_commit
    | _ -> 
      log_f ~logger ~level:Notice "\t New entries - nb: %4i, prev index: %10i, leader commit: %10i"
        (List.length r.Raft.rev_log_entries) 
        r.Raft.prev_log_index
        r.Raft.leader_commit
    end 
  
  | Raft.Append_entries_response r-> 
    begin match r.Raft.result with
    | Raft.Success {Raft.receiver_last_log_index} -> 
      log_f ~logger ~level:Notice "\t Success - last log index: %10i" receiver_last_log_index
    | Raft.Log_failure {Raft.receiver_last_log_index; _ } -> 
      log_f ~logger ~level:Notice "\t Failure(Log) - last log index: %10i" receiver_last_log_index 
    | Raft.Term_failure -> 
      log_f ~logger ~level:Notice "\t Failure(Term) - sender term: %i" r.Raft.receiver_term 
    end

let print_msg_to_send logger sender_id msg receiver_id = 
  log_f ~logger ~level:Notice  "Sent [%2i] -> [%2i] : %s" 
    sender_id
    receiver_id
    (string_of_message_type msg) 

  >>= print_msg_details logger msg 

let print_msg_received logger msg receiver_id = 

  let sender_id = match msg with  
    | Raft.Request_vote_request    {Raft.candidate_id; _ } -> candidate_id
    | Raft.Request_vote_response   {Raft.voter_id; _ } -> voter_id
    | Raft.Append_entries_request  {Raft.leader_id; _ } -> leader_id 
    | Raft.Append_entries_response {Raft.receiver_id; _ } -> receiver_id 
  in

  log_f ~logger ~level:Notice  "Received [%2i] -> [%2i] : %s" 
    sender_id
    receiver_id
    (string_of_message_type msg) 

  >>= print_msg_details logger msg 

let print_leader_state logger = function
  | Raft.Candidate _ | Raft.Follower _ -> Lwt.return_unit 
  | Raft.Leader {Raft.indices} -> 
    log_f ~logger ~level:Notice "State"
    >>=(fun () ->
      Lwt_list.iter_s (fun server_index -> 
        let {
          Raft.server_id;
          next_index;
          match_index;
          local_cache;
          heartbeat_deadline;
          outstanding_request;} = server_index 
        in 
        log_f ~logger ~level:Notice "\tServer Index [%2i]: next: %10i, match: %10i, outstanding?: %b"
          server_id
          next_index
          match_index
          outstanding_request
      ) indices
    )
