open Lwt.Infix 
open Lwt_log_core

module U = Lwt_unix 
module Conf = Raft_udp_conf 

let get_next_client_connection_f logger configuration server_id =

  match Conf.sockaddr_of_server_id `Client configuration server_id with
  | None    -> (fun () -> Lwt.return `Failure) 
  | Some ad ->

    try
      let fd = U.socket U.PF_INET U.SOCK_STREAM 0 in 
      U.bind fd ad; 
      U.listen fd 10;   

      (fun () -> 
        Lwt.catch (fun () ->

        U.accept fd 
        >>=(fun (fd2, ad) -> 
          log ~logger ~level:Notice "New client connection accepted"
          >|=(fun () ->
            `New_client_connection fd2
          )
        )

        ) (* with *) (fun exn ->
          
          (* The accept has failed, this is a critical failure AFAIK
           * so best to return a fatale `Failure.
           *)
          log_f ~logger ~level:Fatal 
            "Error when accepting new client connection, details: %s"
            (Printexc.to_string exn)
          >|=(fun () -> 
            `Failure 
          )
        ) 
      )

    with exn -> (fun () -> 

      log_f ~logger ~level:Fatal 
        "Error initializing TCP listen connection for client IPC. details: %s"
        (Printexc.to_string exn)

      >|=(fun () -> `Failure)
    )

let handle_new_client_connection logger req_push fd : unit Lwt.t = 
  let buffer = Bytes.create 1024 in 

  Lwt.catch (fun () ->

  U.read fd buffer 0 1024 
  >>=(fun nb_bytes_received ->

    log_f ~logger ~level:Notice 
      "New Client message received, nb of bytes: %i" nb_bytes_received
    >|=(fun () -> nb_bytes_received)
  )
  >>=(function 
    | 0 ->
      log ~logger ~level:Warning
        "Client terminated connection early"
      >>=(fun () -> U.close fd)

    | nb_bytes_received when nb_bytes_received = 1024 -> 
    
      log ~logger ~level:Error 
        "Client message is too large... closing connection"
      >>=(fun () -> U.close fd)
        (* Here we have a message too large from the 
         * client. We chose to close the connection
         * defensively, we should log such an event
         * when we passed down the logger. 
         *)

    | nb_bytes_received -> begin  
      let decoder = Pbrt.Decoder.of_bytes buffer in 
      begin match Raft_udp_pb.decode_client_request decoder with
      | req -> Lwt.return @@ req_push (Some req)
      | exception exn -> 
        log_f ~logger ~level:Error 
          "Error decoding client request, details: %s"
          (Printexc.to_string exn)
      end 
      >>=(fun () -> U.close fd)
    end
  )
  ) (* try *) (fun exn  ->
    
    log_f ~logger ~level:Error 
      "Error when reading client data from connection, details: %s"
      (Printexc.to_string exn) 
    
    (* 
     * Not much we can do here, as previously mentioned
     * once the logger is passed it, we can log the 
     * failure, but since the failure is localize to a
     * client connection it is not propagated upstream.
     *)
    >>=(fun () -> U.close fd) 
  ) 


let client_request_stream logger configuration server_id = 

  let next_client_connection_f = 
    get_next_client_connection_f logger configuration server_id
  in  

  let req_stream, req_push, set_ref = Lwt_stream.create_with_reference () in 

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

            let recv_thread = 
              handle_new_client_connection logger req_push fd 
              >|=(fun () -> `Req_done)
            in 
            let next_threads = 
              recv_thread::(next_client_connection_f ())::next_threads
              (* 
               * By appending the threads which reads the client
               * connection we ensure that this thread will be 
               * concurrently processed with the new connections.
               *
               * This ensure that new conncetions can be accepted
               * while a client interaction is not over. When 
               * multiple client connects the communication processing will
               * be concurrent.
               * 
               *) 
            in
            (next_threads, is_failure)
          
          | `Req_done ->
            (* 
             * Indication that a client interaction was terminated, no
             * follow up to do.
             *)
            (next_threads, is_failure)
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

  set_ref @@ loop [next_client_connection_f ()] (); 
  req_stream
