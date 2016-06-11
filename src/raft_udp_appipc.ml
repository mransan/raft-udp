open Lwt_log_core
open Lwt.Infix 

module Pb = Raft_udp_pb 
module U  = Lwt_unix

type send_app_request_f  = Raft_udp_pb.app_request option -> unit 

type t = send_app_request_f * Raft_udp_pb.app_response Lwt_stream.t 

let section = Section.make (Printf.sprintf "%10s" "AppIPC")

let string_of_debug_info ({Pb.raft_server_id;debug_id} : Pb.app_ipc_debug) =  
  Printf.sprintf "{raft server id: %3i, unique id: %5i}" raft_server_id debug_id

let string_of_app_request {Pb.app_request_payload; app_request_debug_info = {Pb.debug_id; _ } } = 
  let payload = match app_request_payload with
    | Pb.Validate_logs {Pb.log_entries} ->
        
      let log_entries = String.concat ", " @@ List.map (fun ({Pb.request_id; _ } : Pb.log_entry) -> 
        request_id
      ) log_entries in 
      Printf.sprintf "Validate_log [%s]" log_entries

    | Pb.Commit_log   {Pb.request_id; _} -> Printf.sprintf "Commit_log(%s)" request_id 
  in
  Printf.sprintf "%s, debug_id: %i" payload debug_id 

let print_app_response logger {Pb.app_response_debug_info; app_response_payload} = 
  log_f ~logger ~level:Notice ~section "debug info: %s" (string_of_debug_info app_response_debug_info)
  >>=(fun () ->
    match app_response_payload with
    | Pb.Commit_log_ack _ -> log_f ~logger ~level:Notice ~section "Commit log Ack" 
    | Pb.Validations {Pb.validations} ->  
      Lwt_list.iter_s (fun {Pb.result;request_id} -> 
        let result = match result with
          | Pb.Success -> "Success"
          | Pb.Failure _-> "Failure"
        in 
        log_f ~logger ~level:Notice ~section "%s - %s" request_id result 
      ) validations 
  )
 
module Event = struct 
  
  (* Type *)

  type e = 
    | Failure of string 
      (* Fatal failure happened during IPC *)

    | Connection_established of Lwt_unix.file_descr
      (* The connection is established to the App server *)

    | App_request  of Pb.app_request 
      (* An App request is requested to be sent *)

    | App_response of Pb.app_response 
      (* An App response is received from the App server *)

  (* Builder functions *)

  let app_response app_response () = 
    App_response app_response 

  let connection_established fd () = 
    Connection_established fd 

  let app_request app_request () = 
    App_request app_request 

  let failure context () = 
    Failure context
  
  let failure_lwt context = 
    Lwt.return (Failure context)

end 

let next_response = 
  let buffer = Bytes.create 1024 in 
    (* TODO make the buffer size configurable *)

  fun logger fd () -> 
   U.read fd buffer 0 1024
   >>=(function
     | 0 ->  
       Event.failure_lwt "Connection closed by App server"

     | received when received = 1024 -> 
       Event.failure_lwt "Response by App server is too large"

     | received -> 
       log_f ~logger ~level:Notice ~section "Response received from app server (size: %i)" received
       >>=(fun () ->

         let decoder = Pbrt.Decoder.of_bytes (Bytes.sub buffer 0 received) in 
         match Pb.decode_app_response decoder with
         | app_response -> (
           log_f ~logger ~level:Notice ~section "Response decoded with success: "
           >>=(fun () -> print_app_response logger app_response) 
           >|= Event.app_response app_response 
         )
         | exception exn -> (
           log_f ~logger ~level:Error ~section "Error decoding app response: %s" 
             (Printexc.to_string exn)

           >|= Event.failure "Error decoding App server response" 
         )
       )
   )

let send_request logger fd app_request = 
  let bytes = 
    let encoder = Pbrt.Encoder.create () in 
    Pb.encode_app_request  app_request encoder; 
    Pbrt.Encoder.to_bytes encoder 
  in 
  let bytes_len = Bytes.length bytes in 
  U.write fd bytes 0 bytes_len
  >>=(function
    | 0 -> Event.failure_lwt "Failed to send request to APP server"
    | n -> 
      assert(n = bytes_len); 
      log_f ~logger ~level:Notice ~section "App request successfully sent (%s)" 
        (string_of_debug_info app_request.Pb.app_request_debug_info )

      >>= next_response logger fd
  )  

let connect logger () = 
  let ad = U.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", 40000) in 
  let fd = U.socket U.PF_INET U.SOCK_STREAM 0 in 
  Lwt.catch (fun () -> 
    U.connect fd ad 
    >>=(fun () -> 
      log ~logger ~level:Notice ~section "Connection established with App server"
    )
    >|= Event.connection_established fd 

  ) (* with *) (fun exn -> 

    log_f ~logger ~level:Error ~section "Error connecting to App server, %s" 
      (Printexc.to_string exn) 
    >|= Event.failure "Error connecting to App server"
  ) 

let get_next_request_f logger request_stream = 

  fun () -> 
    Lwt_stream.get request_stream 
    >>=(function 
      | None -> Event.failure_lwt "Request stream is closed"
      | Some request ->
        log_f ~logger ~level:Notice ~section "New request from stream: %s"
          (string_of_app_request request)
        >|= Event.app_request request 
    )

let make logger configuration stats = 

  let (
    request_stream, 
    push_request_f
  ) = Lwt_stream.create() in 

  let (
    response_stream, 
    push_response_f, 
    set_response_ref
  ) = Lwt_stream.create_with_reference () in 

  let next_request = get_next_request_f logger request_stream in 

  (* Main loop which after the connection with the 
   * App server is established keeps on poping the next 
   * app request from the stream, sends t to the App 
   * server via the established connection then wait for the response. 
   *
   * Once the response comes back it is appended to the response
   * stream for the client of this module to pick it up. 
   *
   * Therefore there is only one main thread in this loop. 
   *)
  let rec loop fd = function
    | Event.Failure s -> 
      log_f ~logger ~level:Error ~section "Failure in app IPC: %s" s 
      >|=(fun () -> push_response_f None)

    | Event.Connection_established fd ->
      next_request () >>= loop (Some fd) 
    
    | Event.App_request request ->   
      begin match fd with 
      | None -> assert(false) 
      | Some fd2 -> 
        send_request logger fd2 request >>= loop fd  
      end 

    | Event.App_response response -> 
      push_response_f (Some response); 
      next_request () >>= loop fd 
  in

  let t : unit Lwt.t  = connect logger () >>= loop None in 
  set_response_ref t; 

  (push_request_f, response_stream)
