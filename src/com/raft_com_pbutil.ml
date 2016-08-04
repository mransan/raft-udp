module App_pb = Raft_app_pb 
module Com_pb = Raft_com_pb 
module Clt_pb = Raft_clt_pb 

let string_of_app_request = function
  | App_pb.Commit_txs {App_pb.txs} ->
      
    let log_entries = String.concat ", \n" @@ List.map (fun ({Com_pb.tx_id; _ } : Com_pb.tx) -> 
      Printf.sprintf "%20s" tx_id 
    ) txs in 
    Printf.sprintf "Commit txs [\n%s]" log_entries


let string_of_app_response = function

  | App_pb.Committed_txs {App_pb.validations} ->  
    let validations = String.concat ",\n" @@ List.map (fun {App_pb.result;tx_id} -> 
      let result = match result with
        | App_pb.Validation_success  -> 
          "Success"
        | App_pb.Validation_failure {App_pb.error_message; _} -> 
          Printf.sprintf "Failure (details: %s)" error_message
      in 
      Printf.sprintf "%20s - %s" tx_id result
    ) validations in 
    Printf.sprintf "Committed txs [\n%s]" validations 

let string_of_client_request = function
  | Clt_pb.Add_tx {Com_pb.tx_id; tx_data} -> 
    Printf.sprintf "{tx_id: %s; tx_data length: %i}" tx_id (Bytes.length tx_data) 
