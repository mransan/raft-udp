open !Lwt_log_core

module APb = Raft_com_pb 

module RPb = Raft_pb
module RTypes = Raft_types

let string_of_ranges ranges = 
  String.concat ", " @@ List.rev_map (fun (from, to_) -> 
    Printf.sprintf "[%i - %i]" from to_
  ) ranges 


let string_of_app_request = function
  | APb.Add_log_entries {APb.log_entries} ->
      
    let rec aux from prev ranges = function
      | [] -> 
        let ranges = string_of_ranges @@ (from, prev)::ranges in 
        Printf.sprintf "Add_log_entries [%s]" ranges

      | {Raft_pb.index; _} :: tl -> 
         if index = prev + 1 
         then aux from index ranges tl 
         else aux index index ((from, prev)::ranges) tl 
    in 
    begin match log_entries with
    | [] -> "Add_log_entries []" 
    | {Raft_pb.index; _}:: tl -> aux index index [] tl 
    end
 | APb.Init -> "Init"

let string_of_app_response = function
  | APb.Add_log_results {APb.results; last_log_index} ->  

    let rec aux from prev ranges = function
      | [] -> 
        let ranges = string_of_ranges @@ (from, prev)::ranges in 
        Printf.sprintf "Add_log_results [%s], lli: %i" ranges
            last_log_index
      | {APb.index; _} :: tl -> 
         if index = prev + 1 
         then aux from index ranges tl 
         else aux index index ((from, prev)::ranges) tl 
    in 
    match results with
    | [] -> Printf.sprintf "Add_log_results [], lli: %i" last_log_index 
    | {APb.index; _}:: tl -> aux index index [] tl 

let string_of_msg msg = 
  match msg with
  | RPb.Request_vote_request r -> 
    Printf.sprintf "RV Req: id:%i, term:%i, last log: (%i, %i)"
      r.RPb.candidate_id
      r.RPb.candidate_term 
      r.RPb.candidate_last_log_index
      r.RPb.candidate_last_log_term

  | RPb.Request_vote_response r-> 
    Printf.sprintf "RV Res: id:%i, term:%i, granted:%b"
      r.RPb.voter_id
      r.RPb.voter_term
      r.RPb.vote_granted

  | RPb.Append_entries_request r-> 
    begin match r.RPb.log_entries with
    | [] -> 
      Printf.sprintf "AP Req: id:%i, [Heartbeat], prev:(%i, %i), ci: %i"
        r.RPb.leader_id 
        r.RPb.prev_log_index
        r.RPb.prev_log_term
        r.RPb.leader_commit
    
    | {RPb.index; _}::_ -> 
      Printf.sprintf "AP Req, id:%i, [Entries(from:%i)], prev:(%i, %i), ci: %i"
        r.RPb.leader_id 
        index
        r.RPb.prev_log_index
        r.RPb.prev_log_term
        r.RPb.leader_commit
    end
  
  | RPb.Append_entries_response r-> 
    begin match r.RPb.result with
    | RPb.Success {RPb.receiver_last_log_index} -> 
      Printf.sprintf "AP Res, id:%i, Success, last: %i"
        r.RPb.receiver_id receiver_last_log_index
    | RPb.Log_failure {RPb.receiver_last_log_index; _ } -> 
      Printf.sprintf "AP Res, id:%i, Failure, last: %i"
        r.RPb.receiver_id receiver_last_log_index
    | RPb.Term_failure -> 
      Printf.sprintf "AP Res, id:%i, Term Failure, term: %i"
        r.RPb.receiver_id r.RPb.receiver_term 
    end

let print_msg_to_send section msg receiver_id = 
  log_f ~section ~level:Notice  "RAFT Msg Sent to %i: %s"
    receiver_id (string_of_msg msg) 

let print_msg_received section msg = 
  log_f ~section ~level:Notice  "RAFT Msg Received: %s"  
    (string_of_msg msg) 

let print_follower () follower_state = 
  let {RTypes.voted_for; current_leader; _} = follower_state in 

  let int_option = function 
    | None -> "X"
    | Some x -> string_of_int x 
  in 

  Printf.sprintf "vf:%s, cl:%s" 
        (int_option voted_for) (int_option current_leader)

let print_leader () leader_state = 
  String.concat ", " @@ List.map (fun follower -> 
    let {
      RTypes.follower_id; 
      next_index; match_index; outstanding_request; _} = follower
    in 
    Printf.sprintf "(%i:[%i|%i|%b])"
        follower_id next_index match_index outstanding_request
  ) leader_state  


let print_candidate () candidate = 
  Printf.sprintf "vote count: %i" candidate.RTypes.vote_count

let print_state section state = 
  let {
    RTypes.server_id;
    current_term;
    commit_index;
    log = raft_log;
    role; _ 
  } = state in

  let print_role (oc:unit) = function
    | RTypes.Follower x -> print_follower oc x 
    | RTypes.Leader x -> print_leader oc x 
    | RTypes.Candidate x -> print_candidate oc x 
  in 

  let role_char = match role with
    | RTypes.Follower _ -> 'F'
    | RTypes.Leader _ -> 'L'
    | RTypes.Candidate _ -> 'C'
  in

  let log_size = Raft_log.last_log_index raft_log in 

  let s = 
    Printf.sprintf "%c(%i), term:%i, ci: %i, size: %i (%a)"
      role_char server_id current_term commit_index log_size print_role role  
  in 
  
  log ~section ~level:Notice s
