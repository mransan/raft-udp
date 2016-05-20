open Lwt.Infix 
open Lwt_log_core

module U     = Lwt_unix 
module Conf  = Raft_udp_conf 
module Stats = Raft_udp_serverstats

type handle = Lwt_unix.file_descr

type client_request = Raft_udp_pb.client_request * handle 

type send_response_f = (Raft_udp_pb.client_response * handle) option -> unit 

let get_next_client_connection_f logger configuration server_id =

  match Conf.sockaddr_of_server_id `Client configuration server_id with
  | None    -> (fun () -> Lwt.return `Failure) 
  | Some ad ->

    let make_acccept_f fd =  fun () -> 
      Lwt.catch (fun () ->
        U.accept fd 
        >>=(fun (fd2, ad) -> 
          log ~logger ~level:Notice "[ClientIPC] New client connection accepted"
          >|=(fun () -> `New_client_connection fd2)
        )
      ) (* with *) (fun exn ->
        
        (* The accept has failed, this is a critical failure AFAIK
         * so best to return a fatale `Failure.
         *)
        log_f ~logger ~level:Fatal 
          "[ClientIPC] Error when accepting new client connection, details: %s"
          (Printexc.to_string exn)
        >|=(fun () -> `Failure)
      ) 
    in

    try

      let fd = U.socket U.PF_INET U.SOCK_STREAM 0 in 
      U.setsockopt fd U.SO_REUSEADDR true; 
      U.bind fd ad; 
      U.listen fd 100;   
      make_acccept_f fd 
    with exn -> (fun () -> 

      log_f ~logger ~level:Fatal 
        "[ClientIPC] Error initializing TCP listen connection for client IPC. details: %s"
        (Printexc.to_string exn)

      >|=(fun () -> `Failure)
    )

type client_connection_event = 
  | Read_ok of U.file_descr 
  | Closed  

let close_client_connection fd () = 
  U.close fd
  >|= (fun () -> Closed)

let read_with_attempts ~attempts fd buffer pos len = 
  let rec aux = function
    | 1 -> U.read fd buffer pos len
    | n -> 
      U.read fd buffer pos len
      >>=(function
        | 0 -> Lwt_unix.sleep 0.01 >>= (fun () -> aux (n -1))
        | n -> Lwt.return n
      )
  in
  aux attempts


let handle_new_client_connection logger req_push fd : client_connection_event Lwt.t = 
  let buffer = Bytes.create 1024 in 

  Lwt.catch (fun () ->

  read_with_attempts ~attempts:10 fd buffer 0 1024 
  >>=(fun nb_bytes_received ->

    log_f ~logger ~level:Notice 
      "[ClientIPC] New Client message received, nb of bytes: %i" nb_bytes_received
    >|=(fun () -> nb_bytes_received)
  )
  >>=(function 
    | 0 ->
      log ~logger ~level:Warning
        "[ClientIPC] Client terminated connection early"
      >>=(close_client_connection fd)

    | nb_bytes_received when nb_bytes_received = 1024 -> 
    
      log ~logger ~level:Error 
        "[ClientIPC] Client message is too large... closing connection"
      >>=(close_client_connection fd) 
        (* Here we have a message too large from the 
         * client. We chose to close the connection
         * defensively, we should log such an event
         * when we passed down the logger. 
         *)

    | nb_bytes_received -> begin  
      let decoder = Pbrt.Decoder.of_bytes buffer in 
      begin match Raft_udp_pb.decode_client_request decoder with
      | req -> 
        begin 
          req_push (Some (req, fd)); 
          Lwt.return (Read_ok fd)
        end

      | exception exn -> 
        log_f ~logger ~level:Error 
          "[ClientIPC] Error decoding client request, details: %s"
          (Printexc.to_string exn)
        >>=(close_client_connection fd)
      end 
    end
  )
  ) (* try *) (fun exn  ->
    
    log_f ~logger ~level:Error 
      "[ClientIPC] Error when reading client data from connection, details: %s"
      (Printexc.to_string exn) 
    
    (* 
     * Not much we can do here, as previously mentioned
     * once the logger is passed it, we can log the 
     * failure, but since the failure is localize to a
     * client connection it is not propagated upstream.
     *)
    >>=(close_client_connection fd)
  ) 

let process_response_stream logger res_stream =
  Lwt_stream.iter_p (fun (response, fd) -> 

    let encoder = Pbrt.Encoder.create () in 
    Raft_udp_pb.encode_client_response response encoder; 
    let buffer = Pbrt.Encoder.to_bytes encoder in 
    let buffer_len = Bytes.length buffer in 
    
    begin 
      Lwt.catch (fun () -> 
        Lwt_unix.write fd buffer 0 buffer_len
        >>=(fun nb_byte_written -> 
          if nb_byte_written <> buffer_len
          then 
            log_f ~logger ~level:Error 
              "[ClientIPC] Error sending client response, byte written: %i, len: %i"
              nb_byte_written buffer_len
            >>=(fun ()  -> Lwt_unix.close fd)
            >>=(fun () -> log ~logger ~level:Notice "[ClientIPC] Client connection closed") 

          else  Lwt.return_unit
        )

      ) (* with *) (fun exn -> 
        log_f ~logger ~level:Error
          "[ClientIPC] Error sending client response, detail: %s" 
          (Printexc.to_string exn)
        >>=(fun () -> Lwt_unix.close fd)
        >>=(fun () -> log ~logger ~level:Notice "[ClientIPC] Client connection closed") 
      ) 
    end
  ) res_stream  

let client_request_stream logger configuration stats server_id = 

  let next_client_connection_f = 
    get_next_client_connection_f logger configuration server_id
  in  

  let req_stream, req_push, set_req_ref = Lwt_stream.create_with_reference () in 

  (*
   * This is the main client IPC loop. 
   *
   * The client IPC is done using TCP. This means that multiple client
   * connection must be concurrently processed along with the main TCP 
   * `listen` connection which accepts new connections. 
   *
   * Therefore our main event loop waits on a list of [Lwt.t] threads. In 
   * this list one of the thread is the accept threads while all the 
   * others are Request processing threads.
   *) 
  let rec loop threads () = 
    Lwt.nchoose_split threads
    >>=(fun (events, non_terminated_threads) -> 

      let next_threads, is_failure = 
        List.fold_left (fun (next_threads, is_failure) event ->
          match event with
          | `Failure ->
            (next_threads, true)

          | `New_client_connection fd -> 
            Stats.tick_new_client_connection stats;

            let recv_thread = 
              handle_new_client_connection logger req_push fd 
              >|=(fun x -> `Req_done x)
            in 
            let next_threads = 
              recv_thread::(next_client_connection_f ())::next_threads
              (* This is where the fundamental logic of handling a TCP 
               * connection is. Each new connection can then be processed
               * concurently with the the next iteration of accept. 
               *)
            in
            (next_threads, is_failure)
          
          | `Req_done Closed->
            (* 
             * Indication that a client interaction was terminated, no
             * follow up to do.
             *)
            (next_threads, is_failure)
          
          | `Req_done (Read_ok fd)->
            let recv_thread = 
              handle_new_client_connection logger req_push fd 
              >|=(fun x -> `Req_done x)
            in 
            (recv_thread::next_threads, is_failure)
        ) (non_terminated_threads, false) events 
      in 

      if is_failure
      then begin 
        (* 
         * In case of fatal failure we notify the 
         * client application that no more client request 
         * will be accepted by closing the stream. 
         *)
        req_push None; 
        Lwt.return `Failure 
      end
      else loop next_threads ()
    )
  in
  set_req_ref @@ loop [next_client_connection_f ()] (); 

  let res_stream, res_push, set_res_ref = Lwt_stream.create_with_reference () in  
  set_res_ref ((process_response_stream logger res_stream):unit Lwt.t); 
  (req_stream, res_push)
