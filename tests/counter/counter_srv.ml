open Lwt.Infix 
open !Lwt_log_core

module APb = Raft_com_pb
module Pb_util = Raft_udp_pbutil
module Conf = Raft_com_conf

module U  = Lwt_unix 

module Counter_srv = Raft_app_srv.Make(struct

  type tx_data = Counter_pb.tx 

  let decode bytes = 
    let decoder = Pbrt.Decoder.of_bytes bytes in 
    Counter_pb.decode_tx decoder 

end)

module State = struct

  type t = (int * int) 
  
  let process _ counter_value process_id = 
    (counter_value, process_id) 
  
  let empty = (-1, -1)

end 

let process_demo_app_request logger (validations, notify) state = 
  Lwt_list.fold_left_s (fun (tx_validations, state) tx -> 

    let {
      Counter_srv.tx_id; 
      tx_data = {Counter_pb.counter_value; process_id};
    } = tx in

    let state = State.process state counter_value process_id in

    log_f ~logger ~level:Notice "Added: (%06i, %6i) from tx_id: %s" 
          counter_value process_id tx_id

    >|= (fun () -> 
      let tx_validation = Raft_app_srv.({
        tx_id; 
        result = Raft_app_srv.Ok; 
      }) in 
      (tx_validation::tx_validations, state) 
    )
  ) ([], state) validations 

  >|=(fun (tx_validations, state) -> 
    notify @@ List.rev tx_validations; 
    state
  ) 

let main configuration log () = 
  begin 
    if log 
    then 
      let file_name = "app.log" in 
      let template  = 
        "$(date).$(milliseconds) [$(level)] [$(section)] : $(message)" 
      in
      Lwt_log.file ~mode:`Truncate ~template ~file_name ()
    else 
      Lwt.return Lwt_log_core.null
  end
  >>=(fun logger -> 

    let request_stream = Counter_srv.start logger configuration  in 

    Lwt_stream.fold_s (fun request state -> 
      process_demo_app_request logger request state
    ) request_stream State.empty

    >|= ignore 
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
