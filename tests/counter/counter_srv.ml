open Lwt.Infix 
open !Lwt_log_core

module APb = Raft_com_pb
module Pb_util = Raft_udp_pbutil
module Conf = Raft_com_conf

module U  = Lwt_unix 

module Counter_srv = Raft_app_srv.Make(struct

  type log_data = Counter_pb.log 

  let decode bytes = 
    let decoder = Pbrt.Decoder.of_bytes bytes in 
    Counter_pb.decode_log decoder 

end)

module State = struct

  type t = (int * int) 
  
  let process _ counter_value process_id = 
    (counter_value, process_id) 
  
  let empty = (-1, -1)

end 

let process_demo_app_request logger (validations, notify) state = 
  Lwt_list.fold_left_s (fun (log_validations, state) log -> 

    let {
      Counter_srv.id; 
      data = {Counter_pb.counter_value; process_id};
    } = log in

    let state = State.process state counter_value process_id in

    log_f ~logger ~level:Notice "Added: (%06i, %6i) from log id: %s" 
          counter_value process_id id

    >|= (fun () -> 
      let log_validation = Raft_app_srv.({
        id; 
        result = Raft_app_srv.Ok; 
      }) in 
      (log_validation::log_validations, state) 
    )
  ) ([], state) validations 

  >|=(fun (log_validations, state) -> 
    notify @@ List.rev log_validations; 
    state
  ) 

let main configuration server_id log () = 
  begin 
    if log 
    then 
      let file_name = Printf.sprintf "app%03i.log" server_id in 
      let template  = 
        "$(date).$(milliseconds) [$(level)] [$(section)] : $(message)" 
      in
      Lwt_log.file ~mode:`Truncate ~template ~file_name ()
    else 
      Lwt.return Lwt_log_core.null
  end
  >>=(fun logger -> 

    let request_stream = Counter_srv.start logger configuration server_id in 

    Lwt_stream.fold_s (fun request state -> 
      process_demo_app_request logger request state
    ) request_stream State.empty

    >|= ignore 
  )

let () = 
  let configuration = Conf.default_configuration () in

  let log = ref false in 
  let log_spec = Arg.Set log  in
  let server_id = ref (-1) in 
  let server_id_spec = Arg.Set_int server_id in 
  
  Arg.parse [
    ("--log", log_spec, " : enable logging");
    ("--id", server_id_spec, " : server id");
  ] (fun _ -> ()) "test.ml";

  assert(!server_id >= 0);
  assert(!server_id < List.length configuration.Conf.app_server_port);

  Sys.set_signal Sys.sigpipe Sys.Signal_ignore ; 
  Lwt_main.run (main configuration !server_id !log ())
