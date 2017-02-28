module APb = Raft_com_pb 

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
