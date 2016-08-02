module APb = Raft_app_pb 

let string_of_app_request = function
  | APb.Commit_txs {APb.txs} ->
      
    let log_entries = String.concat ", \n" @@ List.map (fun ({APb.tx_id; _ } : APb.tx) -> 
      Printf.sprintf "%20s" tx_id 
    ) txs in 
    Printf.sprintf "Commit txs [\n%s]" log_entries


let string_of_app_response = function

  | APb.Committed_txs {APb.validations} ->  
    let validations = String.concat ",\n" @@ List.map (fun {APb.result;tx_id} -> 
      let result = match result with
        | APb.Validation_success  -> 
          "Success"
        | APb.Validation_failure {APb.error_message; _} -> 
          Printf.sprintf "Failure (details: %s)" error_message
      in 
      Printf.sprintf "%20s - %s" tx_id result
    ) validations in 
    Printf.sprintf "Committed txs [\n%s]" validations 

let string_of_client_request = function
  | APb.Add_tx {APb.tx_id; tx_data} -> 
    Printf.sprintf "{tx_id: %s; tx_data length: %i}" tx_id (Bytes.length tx_data) 
