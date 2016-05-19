open Lwt.Infix 

module Conf = Raft_udp_conf 
module U    = Lwt_unix 

let configuration = Conf.default_configuration () 


let test_msg = 
  let client_request = Raft_udp_pb.(Add_log {
    request_id = "Max";
    data = Bytes.of_string "Hi";
  }) in 
  let encoder = Pbrt.Encoder.create () in 
  Raft_udp_pb.encode_client_request client_request encoder; 
  Pbrt.Encoder.to_bytes encoder 
  

let main () = 

  match Conf.sockaddr_of_server_id `Client configuration 0 with
  | None -> 
     Lwt_io.eprintl "No address know for server 0"

  | Some ad -> 

    let fd = U.socket U.PF_INET U.SOCK_STREAM 0 in 
    U.connect fd ad 
    >>=(fun () -> 
      Lwt_io.printf "Connection established..."
    )
    >>=(fun () ->
      U.write fd test_msg 0 (Bytes.length test_msg) 
    ) 
    >>=(fun bytes_written ->
      Lwt_io.printlf "%i bytes written ." bytes_written
    )
    >>=(fun () ->
      let buffer = Bytes.create 1024 in
      U.read fd buffer 0 1024 
    )
    >>=(fun bytes_read ->
      Lwt_io.printlf "%i bytes read ." bytes_read
    )


let () = 
  Lwt_main.run (main ())
