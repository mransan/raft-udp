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
  
let send_log ad () = 
  fun () -> 
    let fd = U.socket U.PF_INET U.SOCK_STREAM 0 in 
    U.connect fd ad 
    >>= (fun () -> 
      U.write fd test_msg 0 (Bytes.length test_msg) 
      >>=(fun _ ->
        let buffer = Bytes.create 1024 in
        U.read fd buffer 0 1024 
        >>=(fun bytes_read ->
          if bytes_read <> 0 && bytes_read <> 1024
          then 
            let decoder = Pbrt.Decoder.of_bytes buffer in 
            let response = Raft_udp_pb.decode_client_response decoder in
            Lwt.return_unit
            (* 
             * Lwt.return @@ Format.(fprintf std_formatter "%a" Raft_udp_pb.pp_client_response response)
             *)
          else 
            Lwt_io.eprintl "Cannot decode response"
        )
      )
    )
    >>=(fun () -> U.close fd)

let main () = 

  match Conf.sockaddr_of_server_id `Client configuration 0 with
  | None -> 
     Lwt_io.eprintl "No address know for server 0"

  | Some ad -> 
    let send_log_f = send_log ad () in
    let rec loop i () =
      send_log_f ()
      >>= (fun ()-> 
        if i  > 100_000
        then Lwt.return_unit
        else Lwt_unix.sleep 0.0001 >>= loop (i + 1)
      )
    in 
    loop 0 ()

let () = 
  Lwt_main.run (main ())
