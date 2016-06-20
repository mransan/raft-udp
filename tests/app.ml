open Lwt.Infix 
open Lwt_log_core

module UPb = Raft_udp_pb
module APb = Raft_app_pb
module Pb_util = Raft_udp_pbutil
module Conf = Raft_udp_conf

module U  = Lwt_unix 


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

    let module Demo_srv = Raft_app_srv.Make(struct
    
      type tx = Demo_pb.tx 
    
      let decode bytes = 
        let decoder = Pbrt.Decoder.of_bytes bytes in 
        Demo_pb.decode_tx decoder 
    
      let counter_values = ref []
    
      let validate {Demo_pb.counter_value; process_id } = 
        let ok = 
          match !counter_values with
          | [] -> true 
          | (last_counter_value,  _ )::_ -> last_counter_value < counter_value 
        in
        if ok 
        then begin 
          counter_values := (counter_value, process_id) :: !counter_values;
          ign_log_f ~logger ~level:Notice "Added: (%06i, %6i) \n" counter_value process_id;
          Raft_app_srv.Ok
        end
        else 
          Raft_app_srv.Error "not a valid counter value"
    
    end) in

    let request_stream = Demo_srv.start logger configuration  in 
    Lwt_stream.iter_s (fun r -> 
      Lwt.return_unit
    ) request_stream
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
