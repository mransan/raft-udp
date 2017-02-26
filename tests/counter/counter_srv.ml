open Lwt.Infix 
open !Lwt_log_core

module APb = Raft_com_pb
module Pb_util = Raft_udp_pbutil
module Conf = Raft_com_conf

module U  = Lwt_unix 

module Srv = Raft_app_srv.Make(struct

  type data = Counter_pb.app_data

  let decode bytes = 
    let decoder = Pbrt.Decoder.of_bytes bytes in 
    Counter_pb.decode_app_data decoder 

  type result = Counter_pb.app_result

  let encode r = 
    let encoder = Pbrt.Encoder.create () in 
    Counter_pb.encode_app_result r encoder; 
    Pbrt.Encoder.to_bytes encoder

end)

module State = struct

  type t = (int * int) 
  
  let process state app_data = 
    let (counter_value, _) = state in 
    let {Counter_pb.increment; process_id} = app_data in 

    match increment with 
    | v when v <= 0 -> 
      let app_result = Counter_pb.({from = -1; to_ = None; }) in 
      (state, app_result) 

    | 1 -> 
      let counter_value = counter_value + 1 in 
      let app_result = Counter_pb.({from = counter_value + 1; to_ = None}) in 
      ((counter_value, process_id), app_result) 

    | _ -> 
      let from = counter_value + 1 in 
      let counter_value = counter_value + increment in 
      let app_result = Counter_pb.({
        from; 
        to_ = Some counter_value; 
      }) in 
      ((counter_value, process_id), app_result) 

  let empty = (-1, -1)

end 

let process_demo_app_request (logs, notify) state = 
  Lwt_list.fold_left_s (fun (results, state) log -> 

    let {Srv.id; index; app_data} = log in

    let state, result = State.process state app_data in

    log_f ~level:Notice "New state: (%06i, %6i) from log id: %s" 
          (fst state) (snd state) id

    >|= (fun () -> 
      let result = Srv.({
        id; 
        index;
        app_result = Some result;
      }) in 

      (result::results, state) 
    )
  ) ([], state) logs

  >|=(fun (log_results, state) -> 
    notify @@ List.rev log_results; 
    state
  ) 

let main configuration server_id log () = 
  let logger_t = 
      if log
      then 
        let basename = Printf.sprintf "app_server_%08i" server_id in 
        Raft_utl_logger.start ~basename ~interval:60 () 
      else begin 
        Lwt_log_core.default := Lwt_log_core.null; 
        Lwt.return_unit
      end 
  in 
  let server_t = 
    let request_stream = Srv.start configuration server_id in 

    Lwt_stream.fold_s (fun request state -> 
      process_demo_app_request request state
    ) request_stream State.empty

    >|= ignore 
  in 

  Lwt_main.run @@ Lwt.join [logger_t; server_t]

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
  main configuration !server_id !log ()
