open Lwt.Infix 
open !Lwt_log_core

module APb = Raft_com_pb
module Pb_util = Raft_udp_pbutil
module Conf = Raft_com_conf

module U  = Lwt_unix 

type connection = (Lwt_io.input_channel * Lwt_unix.file_descr * bytes)  

module  Event = struct 

  type e = 
    | Failure        of string 
      (** Fatal failure of the IPC *)

    | New_connection of connection
      (** TCP IPC accepted a new connection *)

    | New_request    of APb.app_request * connection
      (** New request successfully received and decoded *)

    | App_response of APb.app_response * connection 

    | Connection_close 
      (** A connection was closed *)
    
    | Response_sent of connection 

  let close_connection (ic, _, _) () = 
    Lwt_io.close ic 
    >|= (fun () -> Connection_close) 

  let response_sent connection () = 
    Response_sent connection

  let new_connection connection () = 
    New_connection connection

  let new_request app_request connection () = 
    New_request (app_request, connection) 
end 

let get_next_connection_f logger {Conf.app_server_port; _} server_id =

  let port = 
    match List.nth app_server_port server_id with
    | p -> p 
    | exception _ -> assert(false)
  in 

  (* Initial, done once, connection setup
   *) 
  let ad = U.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", port) in 
  let fd = U.socket U.PF_INET U.SOCK_STREAM 0 in 
  U.setsockopt fd U.SO_REUSEADDR true;
  U.bind fd ad; 
  U.listen fd 1; 

  (* Function to keep accepting new connections
   *)
  fun () -> 
    Lwt.catch (fun () ->
      U.accept fd
      >>=(fun (fd2, ad) ->
        let ic = Lwt_io.of_fd ~mode:Lwt_io.input fd2 in 
        log_f ~logger ~level:Notice 
              "New connection accepted, details: %s" 
              (Raft_utl_unix.string_of_sockaddr ad)
        >|= Event.new_connection (ic, fd2, Bytes.create 1024)
      )
    ) (* with *) (fun exn ->
      let error = 
        Printf.sprintf "Accept failed: %s\n" (Printexc.to_string exn) 
      in 
      Lwt.return (Event.Failure error)  
    )

let decode_request bytes = 
  let decoder = Pbrt.Decoder.of_bytes bytes in 
  APb.decode_app_request decoder 

let next_request logger ((ic, fd, buffer) as connection) =
  Lwt.catch (fun () ->
    Raft_utl_connection.read_msg_with_header ic buffer
    >>= (fun (buffer, len) -> 
      log_f ~logger ~level:Notice "Request received, size: %i" len
      >>=(fun () -> 
        match decode_request (Bytes.sub buffer 0 len) with
        | app_request ->
          log_f ~logger ~level:Notice 
                "Request decoded: %s" 
                (Pb_util.string_of_app_request app_request)
          >|= Event.new_request app_request (ic, fd, buffer)

        | exception exn -> 
          log_f ~logger ~level:Error 
                "Failed to decode request, details: %s" 
                (Printexc.to_string exn)
          >>= Event.close_connection connection
      )
    ) 
  ) (* with *) (fun exn -> 
    log_f ~logger ~level:Error 
          "Failed to read new request, details: %s" 
          (Printexc.to_string exn)
    >>= Event.close_connection connection
  ) 

let send_app_response logger ((_, fd, _) as connection) app_response = 
  (* Encode to bytes *)
  let bytes = 
    let encoder = Pbrt.Encoder.create () in 
    APb.encode_app_response  app_response encoder; 
    Raft_utl_connection.add_length_prefix (Pbrt.Encoder.to_bytes encoder) 
  in
   
  let bytes_len = Bytes.length bytes in 

  let rec aux pos = 
    let len = bytes_len - pos in 
    U.write fd bytes pos len 
    >>=(function
      | 0 -> Event.close_connection connection () 
      | n when n = len -> 
        log_f ~logger ~level:Notice 
              "Response sent (byte length: %i): %s" bytes_len 
              (Pb_util.string_of_app_response app_response)
        >|= Event.response_sent connection
      | n -> aux (pos + n) 
    )  
  in 
  Lwt.catch (fun () -> aux 0) (fun exn -> 
    log_f ~logger ~level:Error 
      "Failed to send response, details: %s, response: %s" 
      (Printexc.to_string exn) 
      (Pb_util.string_of_app_response app_response) 
    >>= Event.close_connection connection
  ) 

let server_loop logger configuration server_id handle_app_request () =

  let next_connection = 
    get_next_connection_f logger configuration server_id 
  in 

  let rec aux threads  = 
    assert([] <> threads); 
      (* There should always be 1 thread to listen to new connection *)

    Lwt.nchoose_split threads 
    >>=(fun (events, non_terminated_threads)  -> 

      List.fold_left (fun non_terminated_threads -> function 
        | Event.Failure error -> (
          Printf.eprintf "App.native: Error, details: %s\n%!" error; 
          exit 1
        ) 

        | Event.New_connection fd -> 
          (next_connection ())::(next_request logger fd)::non_terminated_threads 

        | Event.New_request (request, fd) -> 
          let t = 
            handle_app_request request 
            >|=(fun response -> Event.App_response (response, fd))
          in 
          t::non_terminated_threads 

        | Event.App_response (response, fd) -> 
          (send_app_response logger fd response)::non_terminated_threads

        | Event.Connection_close -> 
          non_terminated_threads
        
        | Event.Response_sent fd -> 
          (next_request logger fd)::non_terminated_threads

      ) non_terminated_threads events
      |> aux 
    )
  in
  aux [next_connection ()] 

type validation_result = 
  | Ok 
  | Error of string 

type log_validation = {
  id : string; 
  result : validation_result; 
} 

module type App_sig  = sig 

  type log_data 

  val decode : bytes -> log_data 

end

module Make(App:App_sig) = struct 
  
  type log = {
    id : string; 
    data : App.log_data;
  } 
  
  type validations = log list * (log_validation list -> unit) 

  let decode_log {Raft_pb.id; data; _} = 
    {id; data = App.decode data}

  let handle_app_request request_push = function
    | APb.Add_log_entries {APb.log_entries} ->
      let log_entries  = List.map decode_log log_entries in 
      let validations_t, validations_u = Lwt.wait () in  
      request_push (Some (log_entries,  (fun r -> Lwt.wakeup validations_u r)));
      validations_t
      >|=(fun validations -> 
        List.map (fun {id; result} ->
          let result = 
            match result with
            | Ok -> 
              APb.Validation_success

            | Error error_message -> 
              APb.(Validation_failure {
                error_message; 
                error_code  = 1;
              })  
          in 
          {APb.result; id}
        ) validations 
      )
      >|=(fun validations -> 
        APb.(Validations {validations}) 
      )

  let start logger configuration server_id =
     let (
       validations_stream, 
       validations_push, 
       validations_set_ref
     ) = Lwt_stream.create_with_reference () in 
     validations_set_ref @@ 
      server_loop logger configuration server_id 
                  (handle_app_request validations_push) (); 
     validations_stream


end 
