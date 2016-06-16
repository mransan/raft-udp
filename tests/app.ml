open Lwt.Infix 
open Lwt_log_core

module UPb = Raft_udp_pb
module APb = Raft_app_pb
module Pb_util = Raft_udp_pbutil
module Conf = Raft_udp_conf

module U  = Lwt_unix 

module Demo_app = Raft_app.Make(struct

  type tx = Demo_pb.tx 

  let decode bytes = 
    let decoder = Pbrt.Decoder.of_bytes bytes in 
    Demo_pb.decode_tx decoder 

  let validate _ = Raft_app.Ok

end)

let main configuration log () = 
  begin 
    if log 
    then 
      let file_name = "app.log" in 
      let template  = "$(date).$(milliseconds) [$(level)] [$(section)] : $(message)" in
      Lwt_log.file ~mode:`Truncate ~template ~file_name ()
    else 
      Lwt.return Lwt_log_core.null
  end
  >>=(fun logger -> 
    Demo_app.start logger configuration 
  )

let () = 
  let configuration = Conf.default_configuration () in

  let log = ref false in 
  let log_spec = Arg.Set log  in
  
  Arg.parse [
    ("--log", log_spec, " : enable logging");
  ] (fun _ -> ()) "test.ml";

  Sys.set_signal Sys.sigpipe Sys.Signal_ignore ; 
  Lwt_main.run (main configuration !log ())
