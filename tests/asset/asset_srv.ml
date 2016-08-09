open Lwt.Infix 
open !Lwt_log_core
module U  = Lwt_unix 

module App_pb = Raft_app_pb
module Pb_util = Raft_com_pbutil
module Conf = Raft_com_conf

module Asset_srv = Raft_app_server.Make(struct 
  
  type tx_data = Asset_pb.tx

  let decode bytes = 
    let decoder = Pbrt.Decoder.of_bytes bytes in 
    Asset_pb.decode_tx decoder 
   
end) (* Asset srv *)

let process_validation_request ~logger app {Asset_srv.tx_data ; tx_id } =  
  (* TODO handle exception ... in a run there was a cryptokit exception 
   * which made the server crashed. 
   *)
  log_f ~logger ~level:Notice "Processing tx: %s" (Asset_pb.show_tx tx_data)
  >>=(fun () ->
    Asset_app.handle_tx ~logger app tx_data 
  )
  >|=(function 
    | Ok app ->
      (app, Raft_app_server.({tx_id; result = Validation_result_ok})) 
    | Error error_msg -> 
      (app, Raft_app_server.({tx_id; result = Validation_result_error error_msg}))
  )

(* 
 * Reminder: The Raft_app_server module API enforces on the client to validate
 * a bulk of request at a time
 *)
let process_validation_requests logger app (validation_requests, send_validations) = 
 
  let rec aux app validation_results = function
    | [] -> 
      send_validations (List.rev validation_results); 
      Lwt.return app 

    | validation_request :: tl -> 
      process_validation_request ~logger app validation_request 
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

let main configuration log server_id () = 
  begin 
    let to_file = if log then Some (Printf.sprintf "app%i.log" server_id) else None in 
    Raft_utl_lwt.make_logger ?to_file ()  
  end
  >>=(fun logger -> 

    match Asset_srv.start logger configuration server_id with
    | None -> 
      Lwt.fail_with "Error starting App server"

    | Some request_stream ->
      Lwt_stream.fold_s (fun request app -> 
        process_validation_requests logger app request 
      ) request_stream (Asset_app.make ())

      >|= ignore 
  )

let () = 
  let configuration = Conf.default_configuration () in

  let log = ref false in 
  let log_spec = Arg.Set log  in

  let id, id_spec = Conf.get_id_cmdline configuration in 
  
  Arg.parse [
    ("--log", log_spec, " : enable logging");
    ("--id", id_spec, " : raft server id");
  ] (fun _ -> ()) "asset_srv.native";

  Sys.set_signal Sys.sigpipe Sys.Signal_ignore ; 
  Lwt_main.run (main configuration !log !id ())
