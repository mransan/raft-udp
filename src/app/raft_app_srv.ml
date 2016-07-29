open Lwt.Infix 
open !Lwt_log_core

module UPb = Raft_udp_pb
module APb = Raft_app_pb
module Pb_util = Raft_udp_pbutil
module Conf = Raft_udp_conf

module U  = Lwt_unix 

module  Event = struct 

  type e = 
    | Failure        of string 
      (** Fatal failure of the IPC *)

    | New_connection of Lwt_unix.file_descr 
      (** TCP IPC accepted a new connection *)

    | New_request    of APb.app_request * Lwt_unix.file_descr  
      (** New request successfully received and decoded *)

    | App_response of APb.app_response * Lwt_unix.file_descr  
      (** The application notified of a response *)

    | Connection_close 
      (** A connection was closed *)
    
    | Response_sent of Lwt_unix.file_descr
      (** The response was sent to the RAFT server *)

  let close_connection fd () = 
    U.close fd >|= (fun () -> Connection_close) 

  let response_sent fd () = 
    Response_sent fd

  let new_connection fd () = 
    New_connection fd

  let new_request app_request fd () = 
    New_request (app_request, fd) 

  let failure msg () = 
    Failure msg 
end 

let get_next_connection_f logger {UPb.app_server_port; _} () =

  (* Initial, done once, connection setup
   *) 
  let ad = U.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", app_server_port) in 
  let fd = U.socket U.PF_INET U.SOCK_STREAM 0 in 
  U.setsockopt fd U.SO_REUSEADDR true;
  U.bind fd ad; 
  U.listen fd 1; 

  (* Function to keep accepting new connections
   *
   * TODO: Only a single active connection should ever be open at 
   * a time based on the protocol between the RAFT server and the App server.
   *)
  fun () -> 
    Lwt.catch (fun () ->
      U.accept fd
      >>=(fun (fd2, ad) ->
        log_f 
          ~logger 
          ~level:Notice 
          "New connection accepted, details: %s" (Raft_utl_unix.string_of_sockaddr ad)
        >|= Event.new_connection fd2 
      )
    ) (* with *) (fun exn ->
      let error = Printf.sprintf "Accept failed: %s\n" (Printexc.to_string exn) in 
      Lwt.return (Event.Failure error)  
    )

let decode_request bytes = 
  let decoder = Pbrt.Decoder.of_bytes bytes in 
  APb.decode_app_request decoder 

let request_buffer_size = 1024 
(* TODO
 *
 * This size will not fit all applications, since the request type is
 * defined by the application, this generic code should not assume 
 * the maximum size of the request. 
 *
 * Simple solution:
 * Make this buffer size configurable. This would fit a lot of application
 * which message size is pretty much constant. However for more dynamic 
 * application which request size might vary this would not work. 
 *
 * Better solution: 
 * Introduce a fixed size message header when the RAFT server is sending 
 * the request. This header would contain the message size of the request 
 * and this code could then adapt the buffer length. 
 *) 

let next_request =

  let buffer = Bytes.create request_buffer_size in
    (*  Only needs to create the buffer once
     *)

  fun logger fd -> 
    Lwt.catch (fun () ->
      U.read fd buffer 0 request_buffer_size
      >>=(function
        | 0 -> 
          log ~logger ~level:Warning "Connection closed by client (read size = 0)" 
          >>= Event.close_connection fd

        | n when n = request_buffer_size -> 
          (* This can happen if the application request is larger than we 
           * expected. As previously mentioned having this size configurable
           * would help. 
           *)
          log ~logger ~level:Warning "Larger then expected request from client... closing connection" 
          (*
          >>= Event.close_connection fd
          *)
          >|= Event.failure "Error reading request"  
          (* Note that falure to read a message means that the message will be
           * lost for this APP server. Since the protocol between RAFT and APP
           * does not yet handle a recovery phase at connection initialization,
           * it is best to exit the server completely.
           *)
          
        | n -> 
          log_f ~logger ~level:Notice "Request received, size: %i" n
          >>=(fun () -> 
            match decode_request (Bytes.sub buffer 0 n) with
            | app_request ->
              log_f ~logger ~level:Notice "Request decoded: %s" (Pb_util.string_of_app_request app_request)
              >|= Event.new_request app_request fd 

            | exception exn -> 
              log_f ~logger ~level:Error "Failed to decode request, details: %s" (Printexc.to_string exn)
              >>= Event.close_connection fd 
              
          )
      ) 
    ) (* with *) (fun exn -> 
      log_f ~logger ~level:Error "Failed to read new request, details: %s" (Printexc.to_string exn)
      >>= Event.close_connection fd
    ) 

let send_app_response logger fd app_response = 
  (* Encode to bytes *)
  let bytes = 
    let encoder = Pbrt.Encoder.create () in 
    APb.encode_app_response  app_response encoder; 
    Pbrt.Encoder.to_bytes encoder 
  in
  let len = Bytes.length bytes in 

  (* Send the bytes *)
  Lwt.catch (fun () -> 
    U.write fd bytes 0 len
    >>=(function
      | 0 -> Event.close_connection fd () 

      | n when n = len -> 
        assert(len = n); 
        log_f ~logger ~level:Notice "Response sent (byte length: %i): %s" n 
          (Pb_util.string_of_app_response app_response)
        >|= Event.response_sent fd 

      | n -> 
        (* 
         * TODO: check the documentation of Lwt to see if the Lwt library
         * implements the loop over the entire data size. If not 
         * we could easily add here a "write" loop to make sure 
         * all the data is sent. 
         *)
        log_f 
          ~logger 
          ~level:Error 
          "Failed to write full response data, total size: %i, written size: %i" len n 
        >>= Event.close_connection fd 
    )  
  ) (* with *) (fun exn -> 
    log_f ~logger ~level:Error "Failed to send response, details: %s, response: %s" 
      (Printexc.to_string exn) 
      (Pb_util.string_of_app_response app_response) 
    >>= Event.close_connection fd 
  ) 

let server_loop logger configuration handle_app_request () =

  let next_connection = get_next_connection_f logger configuration () in 

  let rec aux threads  = 
    assert([] <> threads); 
      (* There should always be 1 thread to listen to new connection
       *)

    Lwt.nchoose_split threads 
    >>=(fun (events, non_terminated_threads)  -> 

      List.fold_left (fun non_terminated_threads -> function 
        | Event.Failure error -> (
          Printf.eprintf "App server internal error: %s\n%!" error; 
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
  | Validation_result_ok 
  | Validation_result_error of string 

type tx_validation_result = {
  tx_id : string; 
  result : validation_result; 
} 

module type App_sig  = sig 

  type tx_data 

  val decode : bytes -> tx_data 

end

module Make(App:App_sig) = struct 
  
  type tx = {
    tx_id : string; 
    tx_data : App.tx_data;
  } 
  
  type validations = tx list * (tx_validation_result list -> unit) 

  let decode_tx {APb.tx_id; APb.tx_data; } = 
    {tx_id; tx_data = App.decode tx_data}

  let handle_app_request request_push = function
    | APb.Commit_txs {APb.txs} ->
      let txs = List.map decode_tx txs in 
      let validations_t, validations_u = Lwt.wait () in  
      request_push (Some (txs, (fun r -> Lwt.wakeup validations_u r)));
      validations_t
      >|=(fun validations -> 
        List.map (fun {tx_id; result} ->
          let result = 
            match result with
            | Validation_result_ok -> 
              APb.Validation_success

            | Validation_result_error error_message -> 
              APb.(Validation_failure {
                error_message; 
                error_code  = 1;
              })  
          in 
          {APb.result; tx_id}
        ) validations 
      )
      >|=(fun validations -> 
        APb.(Committed_txs {validations}) 
      )

  let start logger configuration server_id =
    
    match Conf.server_ipc_configuration configuration server_id with
    | None -> None 
    | Some server_ipc_configuration -> 

       let (
         validations_stream, 
         validations_push, 
         validations_set_ref
       ) = Lwt_stream.create_with_reference () in 

       let handle_app_f = handle_app_request validations_push in 
       let loop_t:unit Lwt.t = 
         server_loop logger server_ipc_configuration handle_app_f () 
       in 
       validations_set_ref @@ loop_t;  
       Some validations_stream

end 
