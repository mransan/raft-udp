open Lwt.Infix 

module Conf = Raft_udp_conf 

module Demo_clt = Raft_app_clt.Make(struct

  type tx = Demo_pb.tx 

  let encode tx = 
    let encoder = Pbrt.Encoder.create () in 
    Demo_pb.encode_tx tx encoder; 
    Pbrt.Encoder.to_bytes encoder

end)


let rec loop client counter_value () = 
  Demo_clt.send client Demo_pb.({
    counter_value; 
    process_id = Unix.getpid (); 
  }) 
  >|=(function
    | Raft_app_clt.Ok -> () 
    | Raft_app_clt.Error msg -> Printf.eprintf "Error, details: %s\n" msg
  )
  >>= loop client (counter_value + 1) 

let main log () = 
  begin 
    if log 
    then 
      let file_name = Printf.sprintf "client%i.log" (Unix.getpid ()) in 
      Printf.printf "log file: %s\n%!" file_name;
      let template  = "$(date).$(milliseconds) [$(level)] [$(section)] : $(message)" in
      Lwt_log.file ~mode:`Truncate ~template ~file_name ()
    else 
      Lwt.return Lwt_log_core.null 
  end 
  >>=(fun logger -> 
    Raft_app_clt.make logger (Conf.default_configuration ()) 
  ) 
  >>= (fun client -> loop client 0 ()) 

let () =
  let log = ref false in 
  let log_spec = Arg.Set log  in
  
  Arg.parse [
    ("--log", log_spec, " : enable logging");
  ] (fun _ -> ()) "client.ml";

  Random.self_init ();
  Lwt_main.run (main !log ())
