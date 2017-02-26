open Lwt.Infix

module Srv = Raft_app_srv.Make(struct

  type data = Hash_pb.app_data 
  
  let decode bytes = 
    let decoder = Pbrt.Decoder.of_bytes bytes in 
    Hash_pb.decode_app_data decoder 

  type result = Hash_pb.app_result

  let encode r = 
    let encoder = Pbrt.Encoder.create () in 
    Hash_pb.encode_app_result r encoder; 
    Pbrt.Encoder.to_bytes encoder

end)

let process_app_request (logs, notify) state = 
  Lwt_list.fold_left_s (fun (results, state) log -> 
    let {Srv.index; id; app_data = {Hash_pb.random_data}} = log in 
    
    let (prev_index, hash) = state in 
    let hash = Digest.string (hash ^ random_data) |> Digest.to_hex in
      (* The state update is simple the hash of the random data combined
       * with the previous state *)
    if prev_index <> index - 1
    then begin
      Printf.printf "Invalid next index: %i, prev: %i\n%!" index prev_index
    end;

    let state = (index, hash) in 

    let app_result = {Hash_pb.log_index = index; hash; } in 
    let log_result = {Srv.id; index; app_result = Some app_result} in  

    (
      (* simple logging to periodically check all RAFT servers are
       * identical *)
      if index mod 100_000 = 0
      then Lwt_io.printlf "hash(%i) = %s" index hash 
      else Lwt.return_unit 
    )

    >|= (fun () -> (log_result::results, state))

  ) ([], state) logs

  >|=(fun (log_results, state) -> 
    notify @@ List.rev log_results;
    state
  ) 

let main () = 

  (* For simplicity this app has no logging *)
  Lwt_log_core.default := Lwt_log_core.null;

  (* Server id is required *)
  let server_id = ref (-1) in 
  let server_id_spec = Arg.Set_int server_id in 
  Arg.parse [
    ("--id", server_id_spec, " : server id");
  ] (fun _ -> ()) "test.ml";
  assert(!server_id <> -1);

  let configuration = Raft_com_conf.default_configuration () in 

  Lwt_stream.fold_s 
      process_app_request 
      (Srv.start configuration !server_id) 
      (0, "")

let () = ignore @@ Lwt_main.run @@ main () 
