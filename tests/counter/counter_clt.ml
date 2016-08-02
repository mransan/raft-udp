open Lwt.Infix 
open Lwt_log_core 

module Conf = Raft_com_conf 

module Counter_clt = Raft_app_clt.Make(struct

  type tx = Counter_pb.tx 

  let encode tx = 
    let encoder = Pbrt.Encoder.create () in 
    Counter_pb.encode_tx tx encoder; 
    Pbrt.Encoder.to_bytes encoder

end)

let rec loop logger client counter_value () = 
  Counter_clt.send client Counter_pb.({
    counter_value; 
    process_id = Unix.getpid (); 
  }) 
  >>=(function
    | Raft_app_clt.Send_result_ok -> Lwt.return_unit 
    | Raft_app_clt.Send_result_error msg -> 
      log_f ~logger ~level:Warning "Error, details: %s\n" msg
  )
  >>= loop logger client (counter_value + 1) 

let main log () = 
  let to_file = 
    if log 
    then Some (Printf.sprintf "client%i.log" (Unix.getpid ())) 
    else None
  in 
  Raft_utl_lwt.make_logger ?to_file () 
  >>=(fun logger -> 
    Raft_app_clt.make logger (Conf.default_configuration ()) 
    >>= (fun client -> loop logger client 0 ()) 
  ) 

let () =
  let log = ref false in 
  let log_spec = Arg.Set log  in
  
  Arg.parse [
    ("--log", log_spec, " : enable logging");
  ] (fun _ -> ()) "client.ml";

  Random.self_init ();
  Lwt_main.run (main !log ())
