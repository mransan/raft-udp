open Lwt.Infix
open !Lwt_log_core 

module Conf = Raft_com_conf
module U    = Lwt_unix
module App_pb = Raft_app_pb
module Com_pb = Raft_com_pb

let section = Section.make (Printf.sprintf "%10s" "AppClt")

module type App_sig = sig 

  type tx 

  val encode : tx -> bytes 

end 

(* -- Notes on error handling -- 
 *
 * There are 3 type of error this module could have to deal with 
 * a) Fatal error: violation of an invariant or wrong configuration.
 * b) Logical error: the server returns an error as part of adding the tx 
 * c) Internal error: implementation error, this could be due to things like 
 *    bug, lack of memory, corruption of the message on the network, the 
 *    RAFT server not being a leader anymore, loss of connection. 
 *
 *  Finally right now this module does not handle graceful termination.
 *) 

type send_result = 
  | Send_result_app_ok 
  | Send_result_app_error of string 
  | Send_result_internal_error of string 
  | Send_result_failure 

let tx_id_of_client_request = function
  | Raft_clt_pb.Add_tx {Com_pb.tx_id; _} -> tx_id 

module Connection_state = struct 

  type leader =
    | No
      (* No known leader *)

    | Potential of int 
      (* One of the server indicated that this server should be a 
         leader 
       *)
    | Established of int * Lwt_unix.file_descr 
      (* Successful on going connection with the leader *)

  type t = {
    configuration : Com_pb.configuration; 
    leader : leader;
  }

  let string_of_state {leader; _ } = 
    match leader with 
    | No -> "No"
    | Potential i -> Printf.sprintf "Potential(%2i)" i
    | Established (i, _) -> Printf.sprintf "Established(%2i)" i 

  let make configuration = {
    configuration;
    leader = No;
  }
  
  let potential state leader_id = 
    {state with leader = Potential leader_id}

  let establish ({leader; _ } as state) fd = 
    let leader = match leader with
      | Established _ 
      | No          -> failwith "Only potential leader can transition to established"
      | Potential i -> Established (i, fd)
    in 
    {state with leader}

  let next ({configuration; leader} as state) = 
    let nb_of_servers = List.length (configuration.Com_pb.servers_ipc_configuration) in 
    let next = match leader with
      | Established (i, _)  -> (i + 1) mod nb_of_servers
      | No                  -> 0
      | Potential i         -> (i + 1) mod nb_of_servers
    in 

    ({state with leader = Potential next}, next)

end 

type pending_request = Raft_clt_pb.client_request * send_result Lwt.u 

type state = {
  connection_state : Connection_state.t; 
  logger : Lwt_log_core.logger; 
  configuration : Raft_com_pb.configuration; 
  request_stream : pending_request  Lwt_stream.t;
  request_push : pending_request option -> unit;  
  pending_request : pending_request option;
} 

module Event = struct 

  type e = 
    | Connection_established of Lwt_unix.file_descr
      (* Connection established with a server, it does not mean the server
       * is a leader. 
       *)

    | Connection_closed of int option 
      (* The connection with a server was closed. The optional int is for 
       * the potential leader id. It is possible that a RAFT server replied 
       * with Add_log_not_a_leader response which might contain a hint to which 
       * other RAFT server is the current leader. The connection to the server 
       * will be closed but we need to keep track of the hint for the next 
       * potential connection..  
       *)

    | Request_sent of Lwt_unix.file_descr * send_result Lwt.u
      (* A request was sent to the RAFT server on the given file descriptor. 
       * The wakener should be used to notify the client code of the response
       * when it is received. 
       *)
    
    | Response_notified 
      (* The response the client expected was sent back. More request can now
       * be processed. 
       *)

    | Failure of string  
      (* System failure (unexpected) *)

    | Init
      (* Start the client loop *)

  (* 
   * Note that current implementation there is only a single thread processing
   * the request, therefore therefore event are sequential and follow the
   * transitions below
   *
   *          Init 
   *            |
   *            v 
   *     +--- Connection_established ---> Connection_closed
   *     |      |               ^     c)    |   ^ 
   *     V      |a)             +-----------+   |
   *  Failure   |                               |              
   *     ^      v                               |
   *     +--- Request_sent ---------------------+
   *            |    ^
   *            | b) |
   *            V    |
   *           Response_notified 
   *
   * a) Connection to a RAFT server has been successful. Right now 
   *    it does not guarantee that it is the leader. This client must
   *    send one request in order to know if the RAFT server it is 
   *    connected to is a leader or not. This could be improved 
   *    by allowing another pair of request/response in the protocol
   *    to test leadership. 
   *
   * b) Normal operation, the connection established is with the 
   *    leader of the RAFT servers and request/response are being
   *    processed sequentially.
   *    The normal operation will be interupted when either
   *    the RAFT server interupts the connection or is no longer a 
   *    leader.
   *
   * c) This loop of establishing a connection sending a request 
   *    and closing the connection before starting again happens 
   *    when we need to try each RAFT server to find which one is a
   *    leader. Since election of a new leader can take arbitrary 
   *    amount of time, this loop can repeat itself.  
   *)

  (* {2 Creator functions} *) 

  let failure state context () = 
    (state, Failure context)

  let failure_lwt state context () = 
    Lwt.return (state, Failure context)

  let connection_closed ~state ?leader_id ~fd () = 
    U.close fd >|= (fun () -> (state, Connection_closed leader_id))  

  let connection_established state fd () = 
    (state, Connection_established fd)

  let response_notified state () = 
    let state = {state with pending_request = None; } in 
    (state, Response_notified) 

  let request_sent state fd response_wakener () = 
    (state, Request_sent (fd, response_wakener))

end 

(* Attempts to establish a TCP connection to the given server. 
 *
 * In case of success the returned event will [Connection_established] 
 * while on failure it will be [Connection_closed]. 
 *) 
let new_connection ({logger; configuration; _ } as state) to_ = 
  match Conf.sockaddr_of_server_id `Client configuration to_ with
  | None -> 
    log_f 
      ~logger 
      ~level:Fatal 
      ~section 
      "Error getting address for server_id: %i" to_
    >|= Event.failure state "Error gettting address"

  | Some ad ->
    let fd = U.socket U.PF_INET U.SOCK_STREAM 0 in
    Lwt.catch (fun () ->
      U.sleep 0.25
      >>=(fun () -> U.connect fd ad)
      >>=(fun () -> 
        log_f 
          ~logger 
          ~level:Notice 
          ~section 
          "Connection established successfully with server_id: %i" to_
       >|= Event.connection_established state fd
      ) 
    ) (* with *) (fun exn ->

      log_f 
        ~logger 
        ~level:Warning 
        ~section 
        "Error connecting to server_id: %i, details: %s" 
        to_ (Printexc.to_string exn)
      >>= Event.connection_closed ?leader_id:None ~state ~fd
    )

(* Attempts to send the given [client_request] to the established connection with 
 * the leader. 
 *
 * -- Invariants --
 *
 * The function expects the leader to be established, if not [Failure] event
 * is returned. 
 *)
let handle_request ({connection_state; logger; _ } as state) client_request response_wakener = 
  let {Connection_state.leader; _ } = connection_state in 

  match leader with
  | Connection_state.No | Connection_state.Potential _ -> 
    Event.failure_lwt 
      state 
      "Internal error, requests can only be sent to established leader" ()
    (* Invariant violation *)

  | Connection_state.Established (server_id, fd) -> 
    let encoder = Pbrt.Encoder.create () in
    Raft_clt_pb.encode_client_request client_request encoder;
    let buffer = Pbrt.Encoder.to_bytes encoder in
    let buffer_len = Bytes.length buffer in

    Lwt.catch (fun () ->
      U.write fd buffer 0 buffer_len
      >>= (fun nb_byte_written ->
        if nb_byte_written <> buffer_len
        then 
          log_f 
            ~logger 
            ~level:Notice 
            ~section
            (
            "Request (tx_id: %s) failed to send to server id %i, " ^^ 
            "invalid nb byte written (buffer length: %i, written: %i)" 
            )
            (tx_id_of_client_request client_request) 
            server_id 
            buffer_len
            nb_byte_written
           >|= (fun () ->
             Lwt.wakeup 
               response_wakener 
               (Send_result_internal_error "Unexpected response size"); 
             Event.response_notified state () 
           ) 
        else
          log_f 
            ~logger 
            ~level:Notice 
            ~section
            "Request (tx_id: %s) sent to server id: %i (size: %i)" 
            (tx_id_of_client_request client_request) server_id buffer_len 
          >|= Event.request_sent state fd response_wakener 
      )
    ) (* with *) (fun exn ->
      log_f 
        ~logger 
        ~level:Error 
        ~section 
        "Error in IPC with RAFT server when sending request (tx_id: %s), details: %s" 
        (tx_id_of_client_request client_request)
        (Printexc.to_string exn) 
      >>= Event.connection_closed ?leader_id:None ~state ~fd
    )

let next_request ({request_stream; pending_request; _ } as state) () = 
  begin match pending_request with
  | Some (client_request, response_wakener) -> 
      (* This request was previously sent to a RAFT server which was 
       * not a leader. 
       *)
    handle_request state client_request response_wakener 

  | None ->
       (* No previous request, let's therefore wait for the next
        * one requested by the client code (via the Lwt_stream). 
        *)
    Lwt_stream.get request_stream
    >>=(function
      | None -> 
        Event.failure_lwt state "Request stream is closed" () 
          (* Note that when this module supports shutting down 
           * a connection, such a [None] value will actually be expected
           * and a new event [Shutdown] should then be introduced to 
           * terminate gracefully the [event_loop] recursive function.
           *)

      | Some ((client_request, response_wakener) as pending_request) -> 
        let state = {state with pending_request = Some pending_request} in 
        handle_request state client_request response_wakener
    )
  end

let handle_response ({logger; _ } as state) fd client_response response_wakener = 

  match client_response with
  | Raft_clt_pb.Add_log_success -> 
    Lwt.wakeup response_wakener Send_result_app_ok ; 
    Lwt.return @@ Event.response_notified state () 

  | Raft_clt_pb.Add_log_validation_failure -> 
    Lwt.wakeup response_wakener (Send_result_app_error "Validation failure");
    Lwt.return @@ Event.response_notified state () 

  | Raft_clt_pb.Add_log_not_a_leader {Raft_clt_pb.leader_id} -> 
    log_f 
      ~logger 
      ~level:Notice 
      ~section 
      "Not a leader received, leader hint: [%s]"
      @@ (Raft_utl_option.string_of_option string_of_int leader_id ) 
    >>= Event.connection_closed ~state ?leader_id ~fd

let next_response ({logger; _} as state) fd response_wakener () =  
  let buffer = Bytes.create 1024 in
  U.read fd buffer 0 1024
  >>= Raft_utl_lwt.tap (fun nb_bytes_read ->
    log_f 
      ~logger 
      ~level:Notice 
      ~section 
      "Response received (size: %i)" nb_bytes_read
  )
  >>= (function
    | 0 ->
      log
        ~logger
        ~level:Notice
        ~section
        "RAFT server closed the connection"
      >>= Event.connection_closed ?leader_id:None ~state ~fd 

    | 1024 ->
      Lwt.wakeup 
        response_wakener 
        (Send_result_internal_error "Response is too long"); 
      Lwt.return @@ Event.response_notified state () 

    | nb_bytes_read ->
      let decoder = 
        let buffer = Bytes.sub buffer 0 nb_bytes_read in 
        Pbrt.Decoder.of_bytes (Bytes.sub buffer 0 nb_bytes_read) 
      in
      match Raft_clt_pb.decode_client_response decoder with
      | client_response ->
        handle_response state fd client_response response_wakener 

      | exception exn -> 
        log_f 
          ~logger
          ~level:Warning
          ~section
          "Error decoding response from RAFT server, details: %s" 
          (Printexc.to_string exn)
        >|= (fun () ->
          Lwt.wakeup 
            response_wakener 
            (Send_result_internal_error "Decoding response failure"); 
          Event.response_notified state () 
        )
  )

let handle_failure_event state context = 

  let {
    logger; 
    pending_request;
    request_push; 
    request_stream; 
    _ 
  } = state in 

  request_push None;
    (* Closing the stream will prevent new requests from being 
     * attempted by the client code. 
     *)

  (* Let's now make sure that any pending requests should be handled 
   * by returning a failure. Pending requests could either 
   * be in the [pending_request] state field and/or in the [request_stream] *)
  begin match pending_request with 
  | None -> () 
  | Some (_, response_wakener) -> 
    Lwt.wakeup response_wakener Send_result_failure
  end;

  let pending_requests_in_stream = Lwt_stream.get_available request_stream in 
  List.iter (fun (_, response_wakener) -> 
    Lwt.wakeup response_wakener Send_result_failure
  ) pending_requests_in_stream; 

  log_f 
    ~logger 
    ~level:Error 
    ~section 
    "RAFT client connection failure: %s, no more request will be handled."
    context

let rec client_loop (({logger; connection_state;_} as state) , e) =

  match e with 
  | Event.Failure context -> handle_failure_event state context 

  | Event.Connection_established fd ->
    let connection_state = Connection_state.establish connection_state fd in  
    let state = {state with connection_state} in  
    log_f 
      ~logger 
      ~level:Notice 
      ~section 
      "Connection established, %s" (Connection_state.string_of_state connection_state) 
    >>=(fun () ->
      next_request state () >>= client_loop 
    ) 

  | Event.Request_sent (fd, response_wakener) -> 
     next_response state fd response_wakener () 
     >>= client_loop 

  | Event.Response_notified ->
      next_request state () >>= client_loop 
  
  | Event.Connection_closed leader_id ->
    let connection_state, leader_id = match leader_id with
      | None -> Connection_state.next connection_state
      | Some leader_id -> 
        (Connection_state.potential connection_state leader_id, leader_id) 
    in
    let state = {state with connection_state} in 
    U.sleep 0.25
    >>=(fun () -> new_connection state leader_id)
    >>= client_loop 

  | Event.Init ->
    client_loop (state, Event.Connection_closed None) 

type t = {
  request_push : pending_request option -> unit;  
  client_loop : unit Lwt.t; 
}

let make logger configuration = 
  let connection_state = Connection_state.make configuration in 
  let request_stream, request_push  = Lwt_stream.create () in
  let state = {
    connection_state; 
    logger;
    configuration;
    request_stream;
    pending_request = None;
    request_push;
  } in 
  let t = {
    request_push; 
    client_loop = client_loop (state, Event.Init) 
  } in
  Lwt.return t 

let unique_id = 
  let pid = Unix.getpid () in 
  let uid = ref 0 in 
  fun () ->
    incr uid; 
    Printf.sprintf "%06i|%010i" pid !uid 

module Make(App:App_sig) = struct 

  let send {request_push; _} tx = 
    let t, response_wakener = Lwt.wait () in
    let bytes = App.encode tx  in
    let tx = Raft_clt_pb.(Add_tx {
      Raft_com_pb.tx_id = unique_id (); 
      Raft_com_pb.tx_data = bytes;
    }) in 
    begin match request_push (Some (tx, response_wakener)) with
    | () -> () 
    | exception Lwt_stream.Closed ->  
      Lwt.wakeup response_wakener Send_result_failure
    end;
    t
end
