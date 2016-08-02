open Lwt.Infix
open !Lwt_log_core 

module Conf = Raft_com_conf
module U    = Lwt_unix
module UPb  = Raft_udp_pb
module APb  = Raft_app_pb

let section = Section.make (Printf.sprintf "%10s" "AppClt")

module type App_sig = sig 

  type tx 

  val encode : tx -> bytes 

end 

module Ext = struct

  let string_of_option f = function
    | None   -> "None"
    | Some x -> f x 

end 

type send_result = 
  | Send_result_ok 
  | Send_result_error of string 

let tx_id_of_client_request = function
  | APb.Add_tx {APb.tx_id; _} -> tx_id 

module Event = struct 

  type e = 
    | Connection_established of Lwt_unix.file_descr
      (* Connection established with a server, it does not mean the server
       * is a leader. 
       *)

    | Connection_closed 
      (* The connection with a server closed (brutal) 
       *)

    | Request  of APb.client_request * send_result Lwt.u  
    
    | Response of APb.client_response * send_result Lwt.u  

    | Failure of string  
      (** System failure (unexpected) *)

    | Init
      (** Start the client loop *)

  let failure context () = Failure context

  let failure_lwt context () = Lwt.return (Failure context)

  let connection_closed fd () = 
    U.close fd >|= (fun () -> Connection_closed)  

  let connection_established fd () = 
    Connection_established fd

  let lwt_response client_response response_wakener () = 
    Lwt.return @@ Response (client_response, response_wakener)
  
  let client_request client_request response_wakener () = 
    Request (client_request, response_wakener)

end 

module State = struct 

  type leader =
    | No
      (* No known leader *)

    | Potential   of int 
      (* One of the server indicated that this server should be a 
         leader 
       *)
    | Established of int * Lwt_unix.file_descr 
      (* Successful on going connection with the leader *)

  type t = {
    configuration : UPb.configuration; 
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
    let nb_of_servers = List.length (configuration.UPb.servers_ipc_configuration) in 
    let next = match leader with
      | Established (i, _)  -> (i + 1) mod nb_of_servers
      | No                  -> 0
      | Potential i         -> (i + 1) mod nb_of_servers
    in 

    ({state with leader = Potential next}, next)

end 

type pending_request = APb.client_request * send_result Lwt.u 

type t = {
  mutable state : State.t; 
  logger : Lwt_log_core.logger; 
  configuration : Raft_udp_pb.configuration; 
  request_stream : pending_request  Lwt_stream.t;
  request_push : pending_request option -> unit;  
  mutable client_loop : unit Lwt.t;
  mutable pending_request : pending_request option;
} 

(* Attempts to establish a TCP connection to the give server. 
 *
 * In case of success the returned event will [Connection_established] 
 * while on failure it will be [Connection_closed]. 
 *) 
let new_connection {logger; state; _ } to_  = 
  match Conf.sockaddr_of_server_id `Client (state.State.configuration) to_ with
  | None -> 
    log_f 
      ~logger 
      ~level:Fatal 
      ~section 
      "Error getting address for server_id: %i" to_
    >|= Event.failure "Error gettting address"

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
       >|= Event.connection_established fd
      ) 
    ) (* with *) (fun exn ->

      log_f 
        ~logger 
        ~level:Warning 
        ~section 
        "Error connecting to server_id: %i, details: %s" 
        to_ (Printexc.to_string exn)
      >>= Event.connection_closed fd
    )

(* Attempts to send the given [client_request] to the established connection with 
 * the leader. 
 *
 * In case of success [Response] event is returned. 
 * In case of communication failure, [Connection_closed] is returned. 
 *
 * -- Invariants --
 *
 * The function expects the leader to be established, if not [Failure] event
 * is returned. 
 *)
let handle_request ({state; logger; _ }) client_request response_wakener = 
  let {State.leader; _ } = state in 

  match leader with
  | State.No | State.Potential _ -> 
     Event.failure_lwt "Internal error, requests can only be sent to established leader" ()

  | State.Established (server_id, fd) -> 
    let encoder = Pbrt.Encoder.create () in
    APb.encode_client_request client_request encoder;
    let buffer     = Pbrt.Encoder.to_bytes encoder in
    let buffer_len = Bytes.length buffer in

    Lwt.catch (fun () ->
      U.write fd buffer 0 buffer_len
      >>= (fun nb_byte_written ->
        if nb_byte_written <>  buffer_len
        then 
          log_f 
            ~logger 
            ~level:Notice 
            ~section
            "Request (tx_id: %s) failed to send to server id %i, invalid nb byte written (buffer length: %i, written: %i)" 
            (tx_id_of_client_request client_request) 
            server_id 
            buffer_len
            nb_byte_written
           >>= Event.connection_closed fd
        else
          log_f 
            ~logger 
            ~level:Notice 
            ~section
            "Request (tx_id: %s) sent to server id: %i (size: %i)" 
            (tx_id_of_client_request client_request) server_id buffer_len 
          >>=(fun () -> 
            let buffer = Bytes.create 1024 in
            U.read fd buffer 0 1024
            >>= Raft_utl_lwt.tap (fun bytes_read ->
              log_f 
                ~logger 
                ~level:Notice 
                ~section 
                "Response received (size: %i)" bytes_read
            )
            >>= (fun bytes_read ->
              if bytes_read <> 0 && bytes_read <> 1024
              then
                let decoder = Pbrt.Decoder.of_bytes (Bytes.sub buffer 0 bytes_read) in
                Event.lwt_response (APb.decode_client_response decoder) response_wakener ()
              else
                Event.connection_closed fd () 
            )
          )
      )
    ) (* with *) (fun exn ->
      log_f 
        ~logger 
        ~level:Error 
        ~section 
        "Error in IPC with RAFT server, details: %s" 
        (Printexc.to_string exn) 
      >>= Event.connection_closed fd
    )

let handle_response (_:t) client_response response_wakener = 

  match client_response with
  | APb.Add_log_success -> 
    Lwt.wakeup response_wakener Send_result_ok ; 
    `Next_request  

  | APb.Add_log_validation_failure -> 
    Lwt.wakeup response_wakener (Send_result_error "validation failure");
    `Next_request 

  | APb.Add_log_not_a_leader {APb.leader_id} -> 
    `Not_a_leader leader_id 
    
let next_request ({request_stream; pending_request; _ } as t) () = 
  match pending_request with
  | Some (r, w) -> Lwt.return @@ Event.client_request r w () 
  | None ->
    Lwt_stream.get request_stream
    >|=(function
      | None -> Event.failure "Request stream is closed" () 
      | Some ((client_request, response_wakener) as pending_request) -> 
        t.pending_request <- Some pending_request; 
        Event.client_request client_request response_wakener ()  
    )

let rec client_loop ({logger; state; _} as t) e =

  match e with 
  | Event.Failure context -> (
    Printf.eprintf "Exiting: %s\n%!" context; 
    (exit 1 : unit); 
    Lwt.return_unit;
  )

  | Event.Connection_established fd ->
    t.state <- State.establish state fd;
    log_f 
      ~logger 
      ~level:Notice 
      ~section 
      "Connection established, %s" (State.string_of_state state) 
    >>=(fun () ->
      next_request t () >>= client_loop t 
    ) 

  | Event.Request (client_request, response_wakener) -> 
    handle_request t client_request response_wakener 
    >>=(fun e -> 
      log ~logger ~level:Notice ~section "Request poped" 
      >>= (fun () -> client_loop t e)
    )  

  | Event.Response (client_response, response_wakener) ->  
    begin match handle_response t client_response response_wakener with
    | `Next_request -> 
       t.pending_request <- None;
       next_request t () >>= client_loop t 

    | `Not_a_leader leader_id -> 
      log_f 
        ~logger 
        ~level:Notice 
        ~section 
        "Not a leader received, leader hint : %s"
        @@ (Ext.string_of_option string_of_int leader_id ) 
      >>=(fun () ->
          let state, leader_id = match leader_id with
            | None -> State.next state
            | Some leader_id -> 
              State.potential state leader_id, leader_id 
          in
          t.state <- state; 
          new_connection t leader_id >>= client_loop t  
      )
    end 
  
  | Event.Connection_closed ->
    let state, leader_id = State.next state in  
    t.state <- state;
    U.sleep 0.25
    >>=(fun () -> new_connection t leader_id)
    >>= client_loop t 

  | Event.Init ->
    let state, leader_id = State.next state in 
    t.state <- state; 
    new_connection t leader_id 
    >>= client_loop t 

let make logger configuration = 
  let state = State.make configuration in 
  let request_stream, request_push  = Lwt_stream.create () in
  let t = {
    state; 
    logger;
    configuration;
    request_stream;
    request_push;
    client_loop = Lwt.return_unit; 
    pending_request = None;
  } in 
  t.client_loop <- client_loop t Event.Init; 
  Lwt.return t 

let unique_id = 
  let pid = Unix.getpid () in 
  let uid = ref 0 in 
  fun () ->
    incr uid; 
    Printf.sprintf "%06i|%010i" pid !uid 

module Make(App:App_sig) = struct 

  let send {request_push; _} tx = 
    let t, u = Lwt.wait () in
    let bytes = App.encode tx  in
    let tx = Raft_app_pb.(Add_tx {
      tx_id = unique_id (); 
      tx_data = bytes;
    }) in 
    request_push (Some (tx, u)); 
    t
end
