open Lwt.Infix 

module Conf = Raft_udp_conf 

module Demo_app = Raft_clt.Make(struct

  type tx = Demo_pb.tx 

  let encode tx = 
    let encoder = Pbrt.Encoder.create () in 
    Demo_pb.encode_tx tx encoder; 
    Pbrt.Encoder.to_bytes encoder

end)


let send_single_request logger configuration () =
  Raft_clt.make logger configuration 
  >>=(fun client -> 

    Demo_app.send client {Demo_pb.hello_who = "Maxime please"}
  )  
  >|=(function
    | Raft_clt.Ok -> Printf.printf "OK...\n"
    | Raft_clt.Error msg -> Printf.eprintf "Error, details: %s\n" msg
  )


let main log () = 
  begin 
    if log 
    then 
      let file_name = Printf.sprintf "client%i.log" (Unix.getpid ()) in 
      let template  = "$(date).$(milliseconds) [$(level)] [$(section)] : $(message)" in
      Lwt_log.file ~mode:`Truncate ~template ~file_name ()
    else 
      Lwt.return Lwt_log_core.null 
  end 
  >>=(fun logger -> send_single_request logger (Conf.default_configuration ()) ()) 

let () =
  let log = ref false in 
  let log_spec = Arg.Set log  in
  
  Arg.parse [
    ("--log", log_spec, " : enable logging");
  ] (fun _ -> ()) "client.ml";

  Random.self_init ();
  Lwt_main.run (main !log ())
