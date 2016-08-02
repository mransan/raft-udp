open Lwt.Infix 
open !Lwt_log_core

module UPb = Raft_udp_pb
module APb = Raft_app_pb
module Pb_util = Raft_com_pbutil
module Conf = Raft_com_conf

module U  = Lwt_unix 

type connection = Lwt_unix.file_descr * bytes 

let request_buffer_size = 1024 
(* Default buffer size TODO make this a configuration *) 

let make_connection ~fd () = 
  (fd, Bytes.create request_buffer_size)

module  Event = struct 

  type e = 
    | Failure        of string 
      (** Fatal failure of the IPC *)

    | New_connection of connection 
      (** TCP IPC accepted a new connection *)

    | New_request    of APb.app_request * connection
      (** New request successfully received and decoded *)

    | App_response of APb.app_response * connection
      (** The application notified of a response *)

    | Connection_close 
      (** A connection was closed *)
    
    | Response_sent of connection
      (** The response was sent to the RAFT server *)

  let close_connection (fd, _)  () = 
    U.close fd >|= (fun () -> Connection_close) 

  let response_sent connection () = 
    Response_sent connection

  let new_connection connection () = 
    New_connection connection

  let new_request app_request connection () = 
    New_request (app_request, connection) 

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
        >|= Event.new_connection @@ make_connection ~fd:fd2 () 
      )
    ) (* with *) (fun exn ->
      let error = Printf.sprintf "Accept failed: %s\n" (Printexc.to_string exn) in 
      Lwt.return (Event.Failure error)  
    )

let decode_request bytes = 
  let decoder = Pbrt.Decoder.of_bytes bytes in 
  APb.decode_app_request decoder 


let next_request =

  fun logger ((fd, buffer) as connection) -> 
    Lwt.catch (fun () ->

      Raft_com_appmsg.read_message_header fd
      >>= Raft_utl_lwt.tap (fun header -> 
        log_f 
          ~logger 
          ~level:Notice 
          "Message header decoded, message size: %i"
          (Raft_com_appmsg.message_size header) 
      )
      >>=(fun header ->
        let message_size = Raft_com_appmsg.message_size header in 
        let buffer = 
          if message_size <= request_buffer_size
          then buffer
          else Bytes.create message_size
        in 
        U.read fd buffer 0 message_size
        >|= (fun nb_byte_read -> (buffer, nb_byte_read))
      ) 
      >>=(fun (buffer, nb_byte_read) ->
        match nb_byte_read with
        | 0 -> 
          log ~logger ~level:Warning "Connection closed by client (read size = 0)" 
          >>= Event.close_connection connection

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
              >|= Event.new_request app_request connection

            | exception exn -> 
              log_f ~logger ~level:Error "Failed to decode request, details: %s" (Printexc.to_string exn)
              >>= Event.close_connection connection
          )
      ) 
    ) (* with *) (fun exn -> 
      log_f 
        ~logger 
        ~level:Error 
        "Failed to read new request, details: %s" 
        (Printexc.to_string exn)
      >>= Event.close_connection connection
    ) 

let send_app_response logger connection app_response = 
  Lwt.catch (fun () -> 
    let (fd, _) = connection in 
    (* Encode to bytes *)
    let bytes = 
      let encoder = Pbrt.Encoder.create () in 
      APb.encode_app_response  app_response encoder; 
      Pbrt.Encoder.to_bytes encoder 
    in
    let len = Bytes.length bytes in 

    (* Send the header *)
    Raft_com_appmsg.write_message_header ~message_size:len fd () 
    >>=(fun () ->
      (* Send the bytes *)
      U.write fd bytes 0 len
    )
    >>=(function
      | 0 -> Event.close_connection connection () 

      | n when n = len -> 
        assert(len = n); 
        log_f ~logger ~level:Notice "Response sent (byte length: %i): %s" n 
          (Pb_util.string_of_app_response app_response)
        >|= Event.response_sent connection

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
        >>= Event.close_connection connection
    )  
  ) (* with *) (fun exn -> 
    log_f ~logger ~level:Error "Failed to send response, details: %s, response: %s" 
      (Printexc.to_string exn) 
      (Pb_util.string_of_app_response app_response) 
    >>= Event.close_connection connection
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

        | Event.New_connection connection -> 
          (next_connection ())::(next_request logger connection)::non_terminated_threads 

        | Event.New_request (request, connection) -> 
          let t = 
            handle_app_request request 
            >|=(fun response -> Event.App_response (response, connection))
          in 
          t::non_terminated_threads 

        | Event.App_response (response, connection) -> 
          (send_app_response logger connection response)::non_terminated_threads

        | Event.Connection_close -> 
          non_terminated_threads
        
        | Event.Response_sent connection -> 
          (next_request logger connection)::non_terminated_threads

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
