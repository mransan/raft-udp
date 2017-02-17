module APb = Raft_com_pb 

let string_of_app_request = function
  | APb.Add_log_entries {APb.log_entries} ->
      
    let log_entries = String.concat ", \n" @@ List.map (fun log_entry ->
      Printf.sprintf "%20s" log_entry.Raft_pb.id
    ) log_entries in 
    Printf.sprintf "Add_log_entries [\n%s]" log_entries

let string_of_app_response = function
  | APb.Validations {APb.validations} ->  
    let validations = String.concat ",\n" @@ List.map (fun {APb.result;id} -> 
      let result = match result with
        | APb.Validation_success  -> 
          "Success"
        | APb.Validation_failure {APb.error_message; _} -> 
          Printf.sprintf "Failure (details: %s)" error_message
      in 
      Printf.sprintf "%20s - %s" id result
    ) validations in 
    Printf.sprintf "Validations [\n%s]" validations 
