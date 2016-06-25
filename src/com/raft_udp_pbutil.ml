module APb = Raft_app_pb 

let string_of_app_request = function
  | APb.Validate_txs {APb.txs} ->
      
    let log_entries = String.concat ", \n" @@ List.map (fun ({APb.tx_id; _ } : APb.tx) -> 
      Printf.sprintf "%20s" tx_id 
    ) txs in 
    Printf.sprintf "Validate_log [\n%s]" log_entries

  | APb.Commit_tx {APb.tx_id; _} -> Printf.sprintf "Commit_tx(%s)" tx_id 

let string_of_app_response = function

  | APb.Commit_tx_ack {APb.tx_id} -> 
    Printf.sprintf "Commit_tx_ack(tx: %s)" tx_id

  | APb.Validations {APb.validations} ->  
    let validations = String.concat ",\n" @@ List.map (fun {APb.result;tx_id} -> 
      let result = match result with
        | APb.Validation_success  -> 
          "Success"
        | APb.Validation_failure {APb.error_message; _} -> 
          Printf.sprintf "Failure (details: %s)" error_message
      in 
      Printf.sprintf "%20s - %s" tx_id result
    ) validations in 
    Printf.sprintf "Validations [\n%s]" validations 
