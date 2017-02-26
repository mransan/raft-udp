open Lwt.Infix 
open !Lwt_log_core 

module Conf = Raft_com_conf 

module Clt = Raft_app_clt.Make(struct

  type data = Counter_pb.app_data

  let encode data = 
    let encoder = Pbrt.Encoder.create () in 
    Counter_pb.encode_app_data data encoder; 
    Pbrt.Encoder.to_bytes encoder

  type result = Counter_pb.app_result 

  let decode bytes = 
    let decoder = Pbrt.Decoder.of_bytes bytes in 
    Counter_pb.decode_app_result decoder 

end)

let rec loop client () = 
  Clt.send client Counter_pb.({
    increment = 1; 
    process_id = Unix.getpid (); 
  }) 
  >>=(function
    | None -> assert(false) 
    | Some r -> 
      log_f ~level:Notice 
            "counter_value=%010i" r.Counter_pb.from
  )
  >>= loop client 

let main log () = 
  let logger_t = 
      if log
      then 
        let basename = Printf.sprintf "app_client_%08i" (Unix.getpid ()) in 
        Raft_utl_logger.start ~basename ~interval:60 () 
      else begin 
        Lwt_log_core.default := Lwt_log_core.null; 
        Lwt.return_unit
      end 
  in 

  let client_t = 
    Raft_app_clt.make (Conf.default_configuration ()) 
    >>= (fun client -> loop client ()) 
  in 

  Lwt_main.run @@ Lwt.join [logger_t; client_t] 

let () =
  let log = ref false in 
  let log_spec = Arg.Set log  in
  
  Arg.parse [
    ("--log", log_spec, " : enable logging");
  ] (fun _ -> ()) "client.ml";

  Random.self_init ();
  main !log ()
