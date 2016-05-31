open Lwt.Infix
open Lwt_log_core

module U            = Lwt_unix
module Conf         = Raft_udp_conf
module Server_stats = Raft_udp_serverstats
module Pb           = Raft_udp_pb

type handle = Lwt_unix.file_descr

type client_request = Pb.client_request * handle

type send_response_f = (Pb.client_response * handle) option -> unit 

let section = Section.make (Printf.sprintf "%10s" "ClientIPC")

module Event = struct 
  type e = 
    | Failure of string  
      (* Critial failure, all the threads handling client IPC will be 
       * terminated. The string indicates the failure context. 
       *)
    | New_client_connection of Lwt_unix.file_descr  
      (* In TCP each new client must initiate a dedicated connection, this 
       * event notifies of such new connection (ie file descriptor). 
       *)
    | Client_connection_read_ok of client_request
      (* A new request has been succesfully received and decoded from 
       * a client connection. 
       *)
    | Client_connection_read_closed
      (* An error occured while attempting to read from a client connection, 
       * that connection is now closed. 
       *)
    | Client_connection_write_ok of U.file_descr
      (* A response was successfully sent to the given client connection, 
       * more reads can now be made. 
       *)
    | Client_connection_write_closed
      (* An error occured while attempting to write to a client connection, 
       * that connection is now closed. 
       *)

  let failure context () = 
    Failure context 
  
  let failure_lwt context () = 
    Lwt.return (Failure context)
  
  let new_client_connection fd () = 
    New_client_connection fd

  let client_connection_write_ok fd () = 
    Client_connection_write_ok fd 
  
  let client_connection_read_closed fd () =
    U.close fd 
    >|= (fun () -> Client_connection_read_closed)

  let client_connection_write_closed logger fd () =
    U.close fd
    >>=(fun () ->
      log ~logger ~level:Notice ~section "Client connection closed"
    )
    >|= (fun () -> Client_connection_write_closed) 

end 

let get_next_client_connection_f logger configuration server_id =

  match Conf.sockaddr_of_server_id `Client configuration server_id with
  | None    -> Event.failure_lwt "invalid server id"
  | Some ad ->

    let make_acccept_f fd =  fun () ->
      Lwt.catch (fun () ->
        U.accept fd
        >>=(fun (fd2, ad) ->
          log ~logger ~level:Notice ~section "New client connection accepted"
          >|=(fun () -> Event.New_client_connection fd2)
        )
      ) (* with *) (fun exn ->

        (* The accept has failed, this is a critical failure AFAIK
         * so best to return a fatale `Failure.
         *)
        log_f ~logger ~level:Fatal
          ~section "Error when accepting new client connection, details: %s"
          (Printexc.to_string exn)
        >|=(fun () -> Event.Failure "Accept failure")
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
        ~section "Error initializing TCP listen connection for client IPC. details: %s"
        (Printexc.to_string exn)

      >|= Event.failure "bind/listen failure"
    )


let get_next_client_request_f logger = 
  let buffer = Bytes.create 1024 in

  fun fd  -> 
    Lwt.catch (fun () ->

    U.read fd buffer 0 1024
    >>=(fun nb_bytes_received ->

      log_f ~logger ~level:Notice
        ~section "New Client message received, nb of bytes: %i" nb_bytes_received
      >|=(fun () -> nb_bytes_received)
    )
    >>=(function
      | 0 ->
        log ~logger ~level:Warning
          ~section "Client terminated connection early"
        >>=(Event.client_connection_read_closed fd)

      | nb_bytes_received when nb_bytes_received = 1024 ->

        log ~logger ~level:Error
          ~section "Client message is too large... closing connection"
        >>=(Event.client_connection_read_closed fd)

      | nb_bytes_received -> begin
        let decoder = Pbrt.Decoder.of_bytes buffer in
        begin match Pb.decode_client_request decoder with
        | req ->
          Lwt.return (Event.Client_connection_read_ok (req, fd))

        | exception exn ->
          log_f ~logger ~level:Error
            ~section "Error decoding client request, details: %s"
            (Printexc.to_string exn)
          >>=(Event.client_connection_read_closed fd)
        end
      end
    )
    ) (* try *) (fun exn  ->

      log_f ~logger ~level:Error
        ~section "Error when reading client data from connection, details: %s"
        (Printexc.to_string exn)
      
      >>=(Event.client_connection_read_closed fd)
    )

let create_response_stream logger () =

  let response_stream, response_push = Lwt_stream.create () in  

  let response_stream = 
    Lwt_stream.map_s (fun (response, fd) ->

      let encoder = Pbrt.Encoder.create () in
      Pb.encode_client_response response encoder;
      let buffer = Pbrt.Encoder.to_bytes encoder in
      let buffer_len = Bytes.length buffer in

      Lwt.catch (fun () ->
        Lwt_unix.write fd buffer 0 buffer_len
        >>=(fun nb_byte_written ->
          if nb_byte_written <> buffer_len
          then
            log_f ~logger ~level:Error
              ~section "Error sending client response, byte written: %i, len: %i"
              nb_byte_written buffer_len
            >>= Event.client_connection_write_closed logger fd

          else
            log ~logger ~level:Notice ~section "Response successfully sent"
            >|= Event.client_connection_write_ok fd 
        )

      ) (* with *) (fun exn ->
        log_f ~logger ~level:Error
          ~section "Error sending client response, detail: %s"
          (Printexc.to_string exn)
        >>= Event.client_connection_write_closed logger fd
      )
    ) response_stream
  in 
  let next_response = fun () -> 
    Lwt_stream.get response_stream
    >>=(function
      | None   -> (
        log ~logger ~level:Error ~section "Response stream is closed"
        >|= Event.failure "Response stream closed"
      )
      | Some x -> Lwt.return x
    )
  in
  (next_response, response_push)

type t = client_request Lwt_stream.t * send_response_f 

let make logger configuration stats server_id =

  let next_client_connection = get_next_client_connection_f logger configuration server_id in
  
  let next_client_request = get_next_client_request_f logger in  

  let (
    request_stream, 
    request_push, 
    set_request_ref
  ) = Lwt_stream.create_with_reference () in

  let (
    next_response, 
    response_push
  ) = create_response_stream logger  () in 

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
   *
   * Responses computed by the clients are received sequentially using an [Lwt_stream.t]. 
   * They are also sent sequentially (which could be improved).
   *)
  let rec loop threads () =
    Lwt.nchoose_split threads
    >>=(fun (events, non_terminated_threads) ->

      let next_threads, is_failure =
        List.fold_left (fun (next_threads, is_failure) event ->
          match event with
          | Event.Failure context ->
            Printf.eprintf "Client IPC Failure, context: %s\n" context;
            (next_threads, true)

          | Event.New_client_connection fd ->
            Server_stats.tick_new_client_connection stats;

            let recv_thread  = next_client_request fd in
            let next_threads =
              recv_thread::(next_client_connection ())::next_threads
              (* This is where the fundamental logic of handling a TCP
               * connection is. Each new connection can then be processed
               * concurently with the the next iteration of accept.
               *)
            in
            (next_threads, is_failure)

          | Event.Client_connection_read_closed -> 
            (next_threads, is_failure)

          | Event.Client_connection_read_ok r ->
            Server_stats.tick_client_requests stats;
            request_push (Some r);
            (* Since we just got a request from the connection, 
             * we do not read the next request until a response has been
             * sent. (This is the design of the protocol). 
             *
             * The next request thread will be re-added upon the 
             * [Client_connection_write_ok] event. 
             *)
            (next_threads, is_failure)

          | Event.Client_connection_write_ok fd->
            (*
             * After this server write to the client connection, it's
             * expected that the client will send another message, therefore
             * we can now read from that connection.
             *)
            let recv_thread = next_client_request fd in
            ((next_response ())::recv_thread::next_threads, is_failure)

          | Event.Client_connection_write_closed -> 
            ((next_response ())::next_threads, is_failure)
             
        ) (non_terminated_threads, false) events
      in

      if is_failure
      then begin
        (*
         * In case of fatal failure we notify the
         * client application that no more client request
         * will be accepted by closing the stream.
         *)
        request_push None;
        log ~logger ~level:Error ~section "Closing request stream" 
      end
      else loop next_threads ()
    )
  in

  let initial_threads = [
    next_response (); 
    next_client_connection ();
  ] in 

  set_request_ref @@ loop initial_threads ();
    (* We must attach the loop threads to the stream so that the garbage collector
     * does not reclaim the thread. 
     *)

  (request_stream, response_push)
