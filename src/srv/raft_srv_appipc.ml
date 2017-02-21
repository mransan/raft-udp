open Lwt.Infix 
open !Lwt_log_core

module APb = Raft_com_pb
module Pb_util = Raft_udp_pbutil
module Server_stats = Raft_srv_serverstats
module Conf = Raft_com_conf
module U = Lwt_unix

type send_app_request_f  = Raft_com_pb.app_request option -> unit 

type t = send_app_request_f * Raft_com_pb.app_response Lwt_stream.t 

let section = Section.make (Printf.sprintf "%10s" "AppIPC")

type connection = (Lwt_io.input_channel * Lwt_unix.file_descr * bytes)
 
module Event = struct 
  
  (* Type *)

  type e = 
    | Failure of string 
      (* Fatal failure happened during IPC *)

    | Connection_established of connection
      (* The connection is established to the App server *)

    | App_request  of APb.app_request * connection 
      (* An App request is requested to be sent *)

    | App_response of APb.app_response * connection  
      (* An App response is received from the App server *)

  (* Builder functions *)

  let app_response app_response connection ()  = 
    App_response (app_response, connection)  

  let connection_established connection () = 
    Connection_established connection 

  let app_request app_request connection () = 
    App_request (app_request, connection) 

  let failure context () = 
    Failure context
  
  let failure_lwt context = 
    Lwt.return (Failure context)

end 

let connect logger {Conf.app_server_port; _} server_id = 
  let port = 
    match List.nth app_server_port server_id with
    | p -> p 
    | exception _ -> assert(false)
  in 
  let ad = U.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", port) in 
  let fd = U.socket U.PF_INET U.SOCK_STREAM 0 in 

  let rec retry = function
    | 0 -> 
      log_f ~logger ~level:Error ~section "Error connecting to App server" 
      >|= Event.failure "Error connecting to App server"
    | n -> 
      Lwt.catch (fun () -> 
        U.connect fd ad 
        >>=(fun () -> 
          log ~logger ~level:Notice ~section 
              "Connection established with App server"
        )
        >|=(fun () -> 
          let ic = Lwt_io.of_fd ~mode:Lwt_io.input fd in 
          Event.connection_established (ic, fd, Bytes.create 1024) ()
        )
      ) (* with *) (fun exn -> 
        log_f ~logger ~level:Error ~section 
              "Error connecting to App server, %s" 
              (Printexc.to_string exn) 
        >>=(fun () -> 
          Lwt_unix.sleep 1. 
        ) 
        >>= (fun () -> retry (n - 1))
      ) 
  in
  retry 5

let decode_response bytes = 
  let decoder = Pbrt.Decoder.of_bytes bytes in 
  APb.decode_app_response decoder 

let next_response logger configuration server_id (ic, fd, buffer) () = 
  Lwt.catch (fun () ->
    Raft_utl_connection.read_msg_with_header ic buffer 
    >>=(fun (buffer, len) -> 
      log_f ~logger ~level:Notice ~section 
            "Response received from app server (size: %i)" len
      >>=(fun () -> 
        match decode_response (Bytes.sub buffer 0 len) with
        | app_response -> (
          log_f ~logger ~level:Notice ~section 
                "Response decoded with success: %s"
                (Pb_util.string_of_app_response app_response) 
          >|= Event.app_response app_response (ic, fd, buffer) 
        )
        | exception exn -> (
          log_f ~logger ~level:Error ~section 
            "Error decoding app response: %s" 
            (Printexc.to_string exn)

          >|= Event.failure "Error decoding App server response" 
        )
      )
    ) 
  ) (* with *) (fun exn -> 
    log_f ~logger ~level:Error ~section  
          "Failed to read response, details: %s" 
          (Printexc.to_string exn)
    >>= (fun () -> connect logger configuration server_id) 
  ) 

let send_request logger configuration server_id connection app_request = 
  let (_, fd, _) = connection in 

  let bytes = 
    let encoder = Pbrt.Encoder.create () in 
    APb.encode_app_request  app_request encoder; 
    Raft_utl_connection.add_length_prefix (Pbrt.Encoder.to_bytes encoder) 
  in 

  let bytes_len = Bytes.length bytes in 

  let rec aux pos = 
    let len = bytes_len - pos in 
    U.write fd bytes pos len 
    >>=(function
      | 0 -> Event.failure_lwt "Failed to send request to APP server"
      | n when n = len -> 
        log_f ~logger ~level:Notice ~section 
              "App request successfully sent:\n%s" 
              (Pb_util.string_of_app_request app_request)
        >>= next_response logger configuration server_id connection
      | n -> aux (pos + n) 
    )  
  in 
  Lwt.catch (fun () -> aux 0) (fun exn -> 
    log_f ~logger ~level:Error ~section 
          "Error sending app request to app server: %s"
          (Printexc.to_string exn)
    >|= Event.failure (Printexc.to_string exn)
  ) 

let get_next_request_f logger request_stream = 

  fun connection -> 
    Lwt_stream.get request_stream 
    >>=(function 
      | None -> Event.failure_lwt "Request stream is closed"
      | Some request ->
        log_f ~logger ~level:Notice ~section "New request from stream: %s"
          (Pb_util.string_of_app_request request)
        >|= Event.app_request request connection 
    )

let make logger configuration server_id (_:Server_stats.t)= 

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
  let rec loop = function
    | Event.Failure s -> 
      log_f ~logger ~level:Error ~section "Failure in app IPC: %s" s 
      >|=(fun () -> push_response_f None)

    | Event.Connection_established connection ->
      next_request connection >>= loop 
    
    | Event.App_request (request, connection) ->   
      send_request logger configuration server_id connection request 
      >>= loop 

    | Event.App_response (response, connection) -> 
      push_response_f (Some response); 
      next_request connection  >>= loop 
  in

  let t : unit Lwt.t  = 
    connect logger configuration server_id 
    >>= loop 
  in 
  set_response_ref t; 

  (push_request_f, response_stream)
