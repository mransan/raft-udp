open Lwt.Infix 
open !Lwt_log_core
module U  = Lwt_unix 

module UPb = Raft_udp_pb
module APb = Raft_app_pb
module Pb_util = Raft_udp_pbutil
module Conf = Raft_udp_conf

module Asset_srv = Raft_app_srv.Make(struct 
  
  type tx_data = Asset_pb.tx

  let decode bytes = 
    let decoder = Pbrt.Decoder.of_bytes bytes in 
    Asset_pb.decode_tx decoder 
   
end) (* Asset srv *)

let process_validation_request app {Asset_srv.tx_data ; tx_id } =  
  Asset_app.handle_tx app tx_data 
  >|=(function 
    | Ok app ->
      (app, Raft_app_srv.({tx_id; result = Validation_result_ok})) 
    | Error error_msg -> 
        (app, Raft_app_srv.({tx_id; result = Validation_result_error error_msg}))
  )

(* 
 * Reminder: The Raft_app_srv module API enforces on the client to validate
 * a bulk of request at a time
 *)
let process_validation_requests _ (* logger *) app (validation_requests, send_validations) = 
 
  let rec aux app validation_results = function
    | [] -> 
      send_validations (List.rev validation_results); 
      Lwt.return app 

    | validation_request :: tl -> 
      process_validation_request app validation_request 
      >>=(fun (app, validation_result) -> 
        (*
        log_f ~logger ~level:Notice "Application after validation:\n%s"
        (Asset_app.App.show app)
        *)
        Lwt.return_unit
        >>=(fun () -> 
          aux app (validation_result::validation_results) tl 
        )
      ) 
  in 
  aux app [] validation_requests

(* Boiler plate code for an application (ie logger, configuration, ...) *)

let main configuration log () = 
  begin 
    let to_file = if log then Some "app.log" else None in 
    Raft_utl_lwt.make_logger ?to_file ()  
  end
  >>=(fun logger -> 

    let request_stream = Asset_srv.start logger configuration  in 

    Lwt_stream.fold_s (fun request app -> 
      process_validation_requests logger app request 
    ) request_stream (Asset_app.make ())

    >|= ignore 
  )

let () = 
  let configuration = Conf.default_configuration () in

  let log = ref false in 
  let log_spec = Arg.Set log  in
  
  Arg.parse [
    ("--log", log_spec, " : enable logging");
  ] (fun _ -> ()) "asset_srv.native";

  Sys.set_signal Sys.sigpipe Sys.Signal_ignore ; 
  Lwt_main.run (main configuration !log ())
