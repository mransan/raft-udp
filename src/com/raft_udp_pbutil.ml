module APb = Raft_com_pb 

let string_of_app_request = function
  | APb.Add_log_entries {APb.log_entries} ->
      
    let log_entries = String.concat ", \n" @@ List.map (fun log_entry ->
      Printf.sprintf "%20s" log_entry.Raft_pb.id
    ) log_entries in 
    Printf.sprintf "Add_log_entries [\n%s]" log_entries

let string_of_app_response = function
  | APb.Add_log_results {APb.results} ->  
    let results = String.concat ",\n" @@ List.map (fun result  -> 
      let {APb.index; id; result_data} = result in 
      let data_as_string = match result_data with
          | None -> "no result data returned"
          | Some b -> Printf.sprintf "data size: %i" (Bytes.length b)
      in 
      Printf.sprintf "%15i - %20s - %s" index id data_as_string
    ) results in 
    Printf.sprintf "App Results [\n%s]" results
