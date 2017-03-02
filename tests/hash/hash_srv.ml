open Lwt.Infix

module Conf = Raft_com_conf

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
  let env, env_spec = Conf.env_arg in 
  let log = ref false in 
  let log_spec = Arg.Set log  in
  Arg.parse [
    ("--id", server_id_spec, " : server id");
    ("--log", log_spec, " : enable logging");
    ("--env", env_spec, " : which env");
  ] (fun _ -> ()) "hash_srv";
  assert(!server_id <> -1);

  let configuration = Conf.default_configuration !env in 

  let logger_t = 
    if !log
    then 
      let basename = Printf.sprintf "app_server_%08i" !server_id in 
      Raft_utl_logger.start ~basename ~interval:60 () 
    else begin 
      Lwt_log_core.default := Lwt_log_core.null; 
      Lwt.return_unit
    end 
  in 

  let server_t = 
    Lwt_stream.fold_s 
      process_app_request 
      (Srv.start configuration !server_id) 
      (0, "")
    >|= (fun _ -> ())
  in

  Lwt.join [logger_t; server_t]

let () = ignore @@ Lwt_main.run @@ main () 
