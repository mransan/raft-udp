open Lwt.Infix 
open !Lwt_log_core

module RPb = Raft_pb
module RTypes = Raft_types
module RLog = Raft_log

let string_of_message_type = function
  | RPb.Request_vote_request  _   -> "Request Vote Request" 
  | RPb.Request_vote_response _   -> "Request Vote Response"
  | RPb.Append_entries_request  _ -> "Append Entries Request"
  | RPb.Append_entries_response _ -> "Append Entries Response"

let print_msg_details logger section msg () = 
  match msg with
  | RPb.Request_vote_request r -> 
    log_f ~logger ~section ~level:Notice "\t term: %2i - last log: (%2i, %2i)"
      r.RPb.candidate_term 
      r.RPb.candidate_last_log_index
      r.RPb.candidate_last_log_term

  | RPb.Request_vote_response r-> 
    if r.RPb.vote_granted 
    then 
      log_f ~logger ~section ~level:Notice "\t Granted - term: %i"
        r.RPb.voter_term
    else
      log_f ~logger ~section ~level:Notice "\t Rejected - term: %i" 
        r.RPb.voter_term

  | RPb.Append_entries_request r-> 
    begin match r.RPb.rev_log_entries with
    | [] -> 
      log_f ~logger ~section ~level:Notice "\t Heartbeat - prev index: %10i, prev term: %10i leader commit: %10i"
        r.RPb.prev_log_index 
        r.RPb.prev_log_term
        r.RPb.leader_commit
    | _ -> 
      log_f ~logger ~section ~level:Notice "\t New entries - nb: %4i, prev index: %10i, prev term: %10i, leader commit: %10i"
        (List.length r.RPb.rev_log_entries) 
        r.RPb.prev_log_index
        r.RPb.prev_log_term
        r.RPb.leader_commit
    end 
  
  | RPb.Append_entries_response r-> 
    begin match r.RPb.result with
    | RPb.Success {RPb.receiver_last_log_index} -> 
      log_f ~logger ~section ~level:Notice "\t Success - last log index: %10i" receiver_last_log_index
    | RPb.Log_failure {RPb.receiver_last_log_index; _ } -> 
      log_f ~logger ~section ~level:Notice "\t Failure(Log) - receiver last log index: %10i" receiver_last_log_index
    | RPb.Term_failure -> 
      log_f ~logger ~section ~level:Notice "\t Failure(Term) - sender term: %i" r.RPb.receiver_term 
    end

let print_msg_to_send logger section sender_id msg receiver_id = 
  log_f ~logger ~section ~level:Notice  "Sent [%2i] -> [%2i] : %s" 
    sender_id
    receiver_id
    (string_of_message_type msg) 

  >>= print_msg_details logger section msg 

let print_msg_received logger section msg receiver_id = 

  let sender_id = match msg with  
    | RPb.Request_vote_request    {RPb.candidate_id; _ } -> candidate_id
    | RPb.Request_vote_response   {RPb.voter_id; _ } -> voter_id
    | RPb.Append_entries_request  {RPb.leader_id; _ } -> leader_id 
    | RPb.Append_entries_response {RPb.receiver_id; _ } -> receiver_id 
  in

  log_f ~logger ~section ~level:Notice  "Received [%2i] -> [%2i] : %s" 
    sender_id
    receiver_id
    (string_of_message_type msg) 

  >>= print_msg_details logger section msg 

let print_follower () follower_state = 
  let {
    RTypes.voted_for;
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
        RTypes.follower_id;
        next_index;
        outstanding_request; _ 
      } = server_index in 

      Printf.sprintf "\t\t\t\t server index: (id: %3i, next: %10i, out req.: %b)\n%a" 
        follower_id
        next_index
        outstanding_request
        aux tl 
  in
  Printf.sprintf "\t\t %15s: Leader\n%a"
    "role"
    aux leader_state

let print_candidate () candidate_state = 
  let fmt = 
    "\t\t %15s: Candidate\n" ^^ 
    "\t\t\t %15s: %i\n" ^^ 
    "\t\t\t %15s: %f\n" 
  in
  let {
    RTypes.vote_count; 
    RTypes.election_deadline;
  } = candidate_state in
  Printf.sprintf fmt 
    "role"
    "vote count" vote_count
    "elec dead." election_deadline

let print_state logger section state = 
  let {
    RTypes.server_id;
    current_term;
    log = {RLog.log_size; _  };
    commit_index;
    role; _ 
  } = state in

  let print_role (oc:unit) = function
    | RTypes.Follower x -> print_follower oc x 
    | RTypes.Leader x -> print_leader oc x 
    | RTypes.Candidate x -> print_candidate oc x 
  in 

  let fmt = 
    "Raft State:\n"    ^^ 
    "\t\t%15s : %i \n" ^^ 
    "\t\t%15s : %i \n" ^^ 
    "\t\t%15s : %i \n" ^^ 
    "\t\t%15s : %i \n" ^^ 
    "%a"
  in
  log_f ~logger ~section ~level:Notice fmt 
    "id" server_id
    "current term" current_term
    "commit index" commit_index
    "log size " log_size 
    print_role role 
