open Lwt.Infix 
open Lwt_log_core

module UPb = Raft_udp_pb
module APb = Raft_app_pb
module Pb_util = Raft_udp_pbutil
module Conf = Raft_udp_conf

module U  = Lwt_unix 

let string_of_sockaddr = function
  | Unix.ADDR_UNIX addr -> 
    Printf.sprintf "ADDR_UNIX(%s)" addr
  | Unix.ADDR_INET (addr, port) -> 
    Printf.sprintf "ADDR_INET(address: %s, port: %i)" (Unix.string_of_inet_addr addr) port

type validation = 
  | Ok 
  | Error of string 

module type App_sig  = sig 

  type tx 

  val decode : bytes -> tx  

  val validate : tx -> validation 

end

module  Event = struct 

  type e = 
    | Failure        of string 
      (** Fatal failure of the IPC *)

    | New_connection of Lwt_unix.file_descr 
      (** TCP IPC accepted a new connection *)

    | New_request    of APb.app_request * Lwt_unix.file_descr  
      (** New request successfully received and decoded *)

    | App_response of APb.app_response * Lwt_unix.file_descr  

    | Connection_close 
      (** A connection was closed *)
    
    | Response_sent of Lwt_unix.file_descr

  let close_connection fd () = 
    U.close fd >|= (fun () -> Connection_close) 

  let response_sent fd () = 
    Response_sent fd

  let new_connection fd () = 
    New_connection fd

  let new_request app_request fd () = 
    New_request (app_request, fd) 
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
   *)
  fun () -> 
    Lwt.catch (fun () ->
      U.accept fd
      >>=(fun (fd2, ad) ->
        log_f ~logger ~level:Notice "New connection accepted, details: %s" (string_of_sockaddr ad)
        >|= Event.new_connection fd2 
      )
    ) (* with *) (fun exn ->
      let error = Printf.sprintf "Accept failed: %s\n" (Printexc.to_string exn) in 
      Lwt.return (Event.Failure error)  
    )

let decode_request bytes = 
  let decoder = Pbrt.Decoder.of_bytes bytes in 
  APb.decode_app_request decoder 

let next_request  =

  let buffer = Bytes.create 1024 in
    (*  Only needs to create the buffer once
     *)

  fun logger fd -> 
    Lwt.catch (fun () ->
      U.read fd buffer 0 1024 
      >>=(function
        | 0 -> 
          log ~logger ~level:Warning "Connection closed by client (read size = 0)" 
          >>= Event.close_connection fd

        | 1024 -> 
          log ~logger ~level:Warning "Larger then expected request from client... closing connection" 
          >>= Event.close_connection fd
          
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
      | n -> 
        assert(len = n); 
        log_f ~logger ~level:Notice "Response sent (byte length: %i): %s" n 
          (Pb_util.string_of_app_response app_response)
        >|= Event.response_sent fd 
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

module Make(App:App_sig) = struct 
  
  type validations = (string * App.tx ) list * ((string * validation) list -> unit) 

  let decode_tx {APb.tx_id; APb.tx_data; } = 
    (tx_id, App.decode tx_data) 
    (*
    let result     = match App.validate tx with
      | Ok -> APb.Success 
      | Error error_message -> APb.(Failure {
        error_message; 
        error_code  = 1;
      })  
    in
    APb.({result; tx_id; })
   *)

  let handle_app_request request_push = function
    | APb.Validate_txs {APb.txs} ->
      let txs = List.map decode_tx txs in 
      let validations_t, validations_u = Lwt.wait () in  
      request_push (Some (txs, (fun r -> Lwt.wakeup validations_u r)));
      validations_t
      >|=(fun validations -> 
        List.map (fun (tx_id, result) ->
         let result = match result with
           | Ok -> APb.Success 
           | Error error_message -> APb.(Failure {
             error_message; 
             error_code  = 1;
           })  
         in 
         APb.({result; tx_id}) 
       ) validations 
      )
      >|=(fun validations -> 
        APb.(Validations {validations}) 
      )
  
    | APb.Commit_tx {APb.tx_id; _ } -> 
      Lwt.return @@ APb.(Commit_tx_ack {tx_id})


  let start logger configuration =
     let (
       validations_stream, 
       validations_push, 
       validations_set_ref
     ) = Lwt_stream.create_with_reference () in 
     validations_set_ref @@ server_loop logger configuration (handle_app_request validations_push) (); 
     validations_stream


end 
