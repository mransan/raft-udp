open Lwt.Infix 
open !Lwt_log_core 

module Conf = Raft_com_conf 

module Counter_clt = Raft_app_clt.Make(struct

  type data = Counter_pb.log 

  let encode log = 
    let encoder = Pbrt.Encoder.create () in 
    Counter_pb.encode_log log encoder; 
    Pbrt.Encoder.to_bytes encoder

  type result = unit 

  let decode (_:bytes) = () 

end)

let rec loop logger client counter_value () = 
  Counter_clt.send client Counter_pb.({
    counter_value; 
    process_id = Unix.getpid (); 
  }) 
  >|=(function
    | None -> () 
    | Some _ -> assert(false) 
      (* no result is expected *)
  )
  >>= (fun () -> Lwt_unix.sleep 0.001) 
  >>= loop logger client (counter_value + 1) 

let main log () = 
  begin 
    if log 
    then 
      let file_name = Printf.sprintf "client%i.log" (Unix.getpid ()) in 
      Printf.printf "log file: %s\n%!" file_name;
      let template = 
        "$(date).$(milliseconds) " ^ 
        "[$(level)] [$(section)] : $(message)" 
      in
      Lwt_log.file ~mode:`Truncate ~template ~file_name ()
    else 
      Lwt.return Lwt_log_core.null 
  end 
  >>=(fun logger -> 
    Raft_app_clt.make logger (Conf.default_configuration ()) 
    >>= (fun client -> loop logger client 0 ()) 
  ) 

let () =
  let log = ref false in 
  let log_spec = Arg.Set log  in
  
  Arg.parse [
    ("--log", log_spec, " : enable logging");
  ] (fun _ -> ()) "client.ml";

  Random.self_init ();
  Lwt_main.run (main !log ())
