module Pb = Raft_udp_pb 

let string_of_debug_info ({Pb.raft_server_id;debug_id} : Pb.app_ipc_debug) =  
  Printf.sprintf "{raft server id: %3i, unique id: %5i}" raft_server_id debug_id

let string_of_app_request {Pb.app_request_payload; app_request_debug_info} = 
  let payload = match app_request_payload with
    | Pb.Validate_logs {Pb.log_entries} ->
        
      let log_entries = String.concat ", \n" @@ List.map (fun ({Pb.request_id; _ } : Pb.log_entry) -> 
        Printf.sprintf "%20s" request_id
      ) log_entries in 
      Printf.sprintf "Validate_log [\n%s]" log_entries

    | Pb.Commit_log   {Pb.request_id; _} -> Printf.sprintf "Commit_log(%s)" request_id 
  in
  Printf.sprintf "%s,\ndebug_info: %s" payload (string_of_debug_info app_request_debug_info)

let string_of_app_response {Pb.app_response_debug_info; app_response_payload} = 

  let payload = match app_response_payload with 
    | Pb.Commit_log_ack {Pb.request_id} -> 
      Printf.sprintf "Commit_log_ack(request_id: %s)" request_id 

    | Pb.Validations {Pb.validations} ->  
      let validations = String.concat ",\n" @@ List.map (fun {Pb.result;request_id} -> 
        let result = match result with
          | Pb.Success  -> 
            "Success"
          | Pb.Failure {Pb.error_message; _} -> 
            Printf.sprintf "Failure (details: %s)" error_message
        in 
        Printf.sprintf "%20s - %s" request_id result
      ) validations in 
      Printf.sprintf "Validations [\n%s]" validations 
  in
  Printf.sprintf "%s,\ndebug_info: %s" payload (string_of_debug_info app_response_debug_info)
