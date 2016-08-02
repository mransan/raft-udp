open Lwt.Infix

module L = Lwt_log_core 
module U = Lwt_unix
module Conf = Raft_com_conf
module Server_stats = Raft_srv_serverstats
module UPb = Raft_udp_pb
module APb = Raft_app_pb 

let section = L.Section.make (Printf.sprintf "%10s" "ClientIPC")

type connection_uid = int 
(* 
 * Unique identifier for each new TCP connection, helps in debugging
 *)

let get_new_connection_uid = 
  let uid_counter = ref 0 in 
  fun () -> 
    incr uid_counter; 
    !uid_counter 

type connection = (Lwt_unix.file_descr * connection_uid * bytes) 
(* 
 * The connection is made of the file descriptor returned from the [Unix.accept] 
 * call, a unique identifier and a byte array to be re-used for reading 
 * incoming message; this avoid re-allocating the byte array all the time. 
 *) 

type handle = connection

let default_buffer_size = 1024 
(* hard coded default value. This should be configurable for the 
 * application. 
 *)

let make_connection ~fd () = 
  let buffer = Bytes.create default_buffer_size in 
  (fd, get_new_connection_uid (), buffer) 

type client_request = APb.client_request * connection

type send_response_f = (APb.client_response * connection) option -> unit 

module Event = struct 

  type e = 
    | Failure of string  
      (* Critial failure, all the threads handling client IPC will be 
       * terminated. The string indicates the failure context. 
       *)
    | New_client_connection of connection 
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
    | Client_connection_write_ok of connection
      (* A response was successfully sent to the given client connection, 
       * more reads can now be made. 
       *)
    | Client_connection_write_closed
      (* An error occured while attempting to write to a client connection, 
       * that connection is now closed. 
       *)

  (* Creator functions *)

  let failure context () = 
    Failure context 
  
  let failure_lwt context () = 
    Lwt.return (Failure context)
  
  let new_client_connection connection () = 
    New_client_connection connection

  let client_connection_write_ok connection  () = 
    Client_connection_write_ok connection
  
  let client_connection_read_closed (fd, _, _) () =
    U.close fd 
    >|= (fun () -> Client_connection_read_closed)

  let client_connection_write_closed logger (fd, _, _) () =
    U.close fd
    >>=(fun () ->
      L.log ~logger ~level:L.Notice ~section "Client connection closed"
    )
    >|= (fun () -> Client_connection_write_closed) 

end 


(*
 * Create and setup the file descriptor for accepting new client TCP 
 * connections. 
 *)
let accept_fd configuration server_id = 
  match Conf.sockaddr_of_server_id `Client configuration server_id with
  | None    -> 
    Error (Printf.sprintf "Invalid server_id: %i" server_id)

  | Some ad ->
    try
      let fd = U.socket U.PF_INET U.SOCK_STREAM 0 in
      U.setsockopt fd U.SO_REUSEADDR true;
      U.bind fd ad;
      U.listen fd 100;
        (* TODO Make 100 configurable *)
      Ok fd   
    with exn -> Error (Printexc.to_string exn) 

let get_next_client_connection_f logger configuration server_id =

  match accept_fd configuration server_id with
  | Error e -> 
    fun () -> 
        L.log_f 
        ~logger
        ~level:L.Fatal 
        ~section
        "Error when creating accept fd, details: %s" e 
      >>= Event.failure_lwt "Failed to initialize client IPC"

  | Ok fd -> 
    fun () ->
      Lwt.catch (fun () ->
        U.accept fd
        >>=(fun (fd2, ad) ->
          let connection_uid = get_new_connection_uid () in 

          L.log_f 
            ~logger 
            ~level:L.Notice 
            ~section 
            "New client connection accepted: %s, connection uid: %i"
            (Raft_utl_unix.string_of_sockaddr ad) connection_uid

          >|= Event.new_client_connection (make_connection ~fd:fd2 ()) 
        )
      ) (* with *) (fun exn ->

        (* The accept has failed, this is a critical failure AFAIK
         * so best to return a fatale `Failure.
         *)
        L.log_f 
          ~logger 
          ~level:L.Fatal
          ~section "Error when accepting new client connection, details: %s"
          (Printexc.to_string exn)
        >|=(fun () -> Event.Failure "Accept failure")
      )

let get_next_client_request_f logger = 

  fun ((fd, connection_uid, buffer) as connection)  -> 
    Lwt.catch (fun () ->
  
      U.read fd buffer 0 default_buffer_size
      >>= Raft_utl_lwt.tap (fun nb_bytes_read ->
        L.log_f 
          ~logger 
          ~level:L.Notice
          ~section "New Client message received, nb of bytes read: %i, connection uid: %i" 
          nb_bytes_read connection_uid
      )
      >>=(function
        | 0 ->
          L.log 
            ~logger 
            ~level:L.Warning
            ~section 
            "Client terminated connection early"
          >>=(Event.client_connection_read_closed connection)

        | nb_bytes_read when nb_bytes_read = default_buffer_size ->

          L.log 
            ~logger 
            ~level:L.Error
            ~section 
            "Client message is too large... closing connection"
          >>=(Event.client_connection_read_closed connection)

        | nb_bytes_read ->
          let decoder = Pbrt.Decoder.of_bytes (Bytes.sub buffer 0 nb_bytes_read) in
          match APb.decode_client_request decoder with
          | req ->
            L.log_f 
              ~logger 
              ~level:L.Notice
              ~section 
              "Client request successfully decoded, details: %s, connection uid: %i" 
              (Raft_com_pbutil.string_of_client_request req) 
              connection_uid
            >|= (fun () -> Event.Client_connection_read_ok (req, connection))

          | exception exn ->
            L.log_f ~logger ~level:L.Error
              ~section "Error decoding client request, details: %s"
              (Printexc.to_string exn)
            >>=(Event.client_connection_read_closed connection)
      )
    ) (* try *) (fun exn  ->

      L.log_f 
        ~logger 
        ~level:L.Error
        ~section 
        "Error when reading client data from connection, details: %s"
        (Printexc.to_string exn)
      
      >>=(Event.client_connection_read_closed connection)
    )

let create_response_stream logger () =

  let response_stream, response_push = Lwt_stream.create () in  

  let response_stream = 
    Lwt_stream.map_s (fun (response, connection) ->

      let encoder = Pbrt.Encoder.create () in
      APb.encode_client_response response encoder;
      let buffer = Pbrt.Encoder.to_bytes encoder in
      let buffer_len = Bytes.length buffer in

      let fd, connection_uid, _ = connection in

      Lwt.catch (fun () ->
        Lwt_unix.write fd buffer 0 buffer_len
        >>=(fun nb_byte_written ->
          if nb_byte_written <> buffer_len
          then
            L.log_f 
              ~logger 
              ~level:L.Error
              ~section "Error sending client response, byte written: %i, len: %i, connection uid: %i"
              nb_byte_written buffer_len connection_uid
            >>= Event.client_connection_write_closed logger connection

          else
            L.log_f 
              ~logger 
              ~level:L.Notice 
              ~section 
              "Response successfully sent to connection uid: %i"
              connection_uid
            >|= Event.client_connection_write_ok connection
        )

      ) (* with *) (fun exn ->
        L.log_f ~logger ~level:L.Error
          ~section "Error sending client response, detail: %s, connection uid: %i"
          (Printexc.to_string exn) connection_uid
        >>= Event.client_connection_write_closed logger connection
      )
    ) response_stream
  in 
  let next_response = fun () -> 
    Lwt_stream.get response_stream
    >>=(function
      | None   -> (
        L.log ~logger ~level:L.Error ~section "Response stream is closed"
        >|= Event.failure "Response stream closed"
      )
      | Some x -> Lwt.return x
    )
  in
  (next_response, response_push)

type t = client_request Lwt_stream.t * send_response_f 


(*
 * Notes on the threading design used in the Client IPC 
 * ---
 *
 * I) Logical decoupling of request handling. 
 *
 * The handling of the request is handled solely by the client of this module. 
 * This module is only concerned with client connection management and
 * decoding/encoding requests/responses. 
 *
 * II) Concurrent processing 
 *
 * In order to also reflect the decoupling above in the threading model, 
 * 2 streams are used:
 * - Request stream: used to push incoming requests to this module client. 
 *   If the module client can't keep up with the flow of request then 
 *   this stream will keep on growing 
 * - Response stream: used by this module client code to queue responses
 *   to be sent. The IPC event loop below will keep on dequeing this 
 *   stream and send each response to the IPC client. 
 *
 * This design allows concurrent IPC computation (connection management, 
 * encoding/decoding..) from request handling (business logic). For instance
 * new requests can be decoded while previous one are being handled by this 
 * module client. 
 *
 * III) Request/Response protocol
 *
 * The protocol between the client process and this server is that each 
 * request has exactly one response. Therefore reading from a client connection
 * is only done either right after the connection is initially established or 
 * when a response has been sent.
 *
 * IV) Threads 
 *
 * - 1 new_connection thread: This threads waits on new connection to be
 * established. 
 * - 1 response thread: This thread keeps poping response from the response
 * stream and send them iteratively
 * - n request thread: Those threads wait on new requests on established
 * connection. 
 *
 * The minimum number of thread is therefore 2 when all client connections
 * have sent new request which have been pushed to the request stream and being
 * processed by this module client. 
 *
 * The maximum number of thread is therefore n + 2 where n is the number of 
 * established client connection. All responses have been sent and it's now
 * up to the client to send the next request. 
 *
 * V) Alternative design 
 *
 * Sending responses (encoding + writing to file descriptor) is done 
 * serially. Each response is poped from the response stream and processed
 * using [Lwt_stream.map_s]. 
 * [Lwt.wait] could be used instead of a stream. This means that the main 
 * event loop will have [1 + n + m] threads (with n threads to read to 
 * wait/read new requests and m threads to wait/write new responses and n 
 * + m = total number of client connections). The effect would be that 
 * responses would be processed concurrently but the main loop would manage 
 * a larger number of thread. This could be particularly important if 
 * one write takes a long time.
 * 
 *) 

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

          | Event.New_client_connection connection ->
            Server_stats.tick_new_client_connection stats;

            let recv_thread  = next_client_request connection in
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

          | Event.Client_connection_write_ok connection ->
            (*
             * After this server write to the client connection, it's
             * expected that the client will send another message, therefore
             * we can now read from that connection.
             *)
            let recv_thread = next_client_request connection in
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
        L.log ~logger ~level:L.Error ~section "Closing request stream" 
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
