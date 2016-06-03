open Lwt.Infix 

module Pb= Raft_udp_pb
module U = Lwt_unix 

let get_next_connection () =

  let ad = U.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", 40000) in 
  let fd = U.socket U.PF_INET U.SOCK_STREAM 0 in 
  U.setsockopt fd U.SO_REUSEADDR true;
  U.bind fd ad; 
  U.listen fd 1; 
  Lwt.catch (fun () ->
    U.accept fd
    >|=(fun (fd2, ad) ->
      Printf.printf "New connection accepted\n"; 
      fd2
    )
  ) (* with *) (fun exn ->
    Printf.eprintf "Accept failed: %s\n" (Printexc.to_string exn); 
    Lwt.fail exn  
  )

type incoming_message =
  | Receive_error   of string 
  | Request of Pb.app_request 

let receive_error_lwt s () = Lwt.return (Receive_error s) 

let request_lwt r () = Lwt.return (Request r)

let decode_request bytes = 
  let decoder = Pbrt.Decoder.of_bytes bytes in 
  Pb.decode_app_request decoder 

let get_next_request fd =
  let buffer = Bytes.create 1024 in

  Lwt.catch (fun () ->
    U.read fd buffer 0 1024 
    >>=(fun _ -> request_lwt (decode_request buffer) ()) 
  ) (* with *) (fun exn -> 
    receive_error_lwt (Printf.sprintf "failed to read: %s" (Printexc.to_string exn)) ()
  ) 

let handle_app_request = function  
  | Pb.Validate_log {Pb.request_id; data; } -> 
    if Bytes.length data > 10 
    then Pb.(Failure {
      error_message = "Data is too big"; 
      error_code = 1;
    }) 
    else Pb.Success
  | Pb.Commit_log _ -> Pb.Success

let bytes_of_response app_response = 
  let encoder = Pbrt.Encoder.create () in 
  Pb.encode_app_response  app_response encoder; 
  Pbrt.Encoder.to_bytes encoder 

type send_result =
  | Ok 
  | Send_error of string 

let send_error error_message = Send_error error_message

let send_app_response fd app_response = 
  let bytes = bytes_of_response app_response in 
  U.write fd bytes 0 (Bytes.length bytes) 
  >|= (function
    | 0 -> send_error "Connection terminated by clien!"  
    | _ -> Ok
  )  

let server_loop () =
  
  get_next_connection ()
  >>=(fun fd -> 
    let rec aux () = 
      get_next_request fd
      >|=(function
        | Receive_error s -> Printf.eprintf "Error: %s\n" s; exit 1
        | Request r -> r 
      )  
      >|= handle_app_request 
      >>= send_app_response fd 
      >>=(function
        | Send_error s -> Printf.eprintf "Error:%s\n" s; exit 1 
        | Ok -> aux ()
      )
    in 
    aux ()
  ) 

let () = 
  Lwt_main.run (server_loop ())
