
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

let print_follower () follower_state = 
  let {
    Raft_pb.voted_for;
    current_leader;
    election_deadline;} = follower_state in 

  let int_option = function 
    | None -> "None"
    | Some x -> Printf.sprintf "Some(%i)" x
  in 

  let fmt = 
    "\t\t %15s: Follower\n"     ^^ 
    "\t\t\t %15s : %s\n"  ^^ 
    "\t\t\t %15s : %s\n"  ^^ 
    "\t\t\t %15s : %f\n"
  in 

  Printf.sprintf fmt 
    "role"
    "voted for" (int_option voted_for)
    "current leader" (int_option current_leader)
    "election d." election_deadline

let print_leader () leader_state = 

  let rec aux () = function
    | [] -> ""
    | server_index::tl -> 
      let {
        Raft_pb.server_id : int;
        next_index : int;
        outstanding_request : bool;
      } = server_index in 

      Printf.sprintf "\t\t\t\t server index: (id: %i, next: %i, out req.: %b)\n%a" 
        server_id
        next_index
        outstanding_request
        aux tl 
  in
  Printf.sprintf "\t\t %15s: Leader\n%a"
    "role"
    aux leader_state.Raft_pb.indices

let print_candidate () candidate_state = 
  let fmt = 
    "\t\t %15s: Candidate\n" ^^ 
    "\t\t\t %15s: %i\n" ^^ 
    "\t\t\t %15s: %f\n" 
  in
  let {
    Raft_pb.vote_count; 
    Raft_pb.election_deadline;
  } = candidate_state in
  Printf.sprintf fmt 
    "role"
    "vote count" vote_count
    "elec dead." election_deadline

let print_state logger state = 
  let {
    Raft_pb.id;
    current_term;
    log;
    log_size;
    commit_index;
    role;
    configuration;
    global_cache;
  } = state in

  let print_role (oc:unit) = function
    | Raft_pb.Follower x -> print_follower oc x 
    | Raft_pb.Leader x -> print_leader oc x 
    | Raft_pb.Candidate x -> print_candidate oc x 
  in 

  let fmt = 
    "Raft State:\n"    ^^ 
    "\t\t%15s : %i \n" ^^ 
    "\t\t%15s : %i \n" ^^ 
    "\t\t%15s : %i \n" ^^ 
    "\t\t%15s : %i \n" ^^ 
    "%a"
  in
  log_f ~logger ~level:Notice fmt 
    "id" id 
    "current term" current_term
    "commit index" commit_index
    "log size " log_size 
    print_role role 
