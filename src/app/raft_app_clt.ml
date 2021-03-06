open Lwt.Infix
open !Lwt_log_core 

module Conf = Raft_com_conf
module U    = Lwt_unix
module APb  = Raft_com_pb

module type App_sig = sig 
  type data
  val encode : data -> bytes 
  type result 
  val decode : bytes -> result 
end 

module Ext = struct
  let string_of_option f = function
    | None   -> "None"
    | Some x -> f x 
end 

type result = bytes option

type connection = (Lwt_io.input_channel * Lwt_unix.file_descr * bytes)  

module Event = struct 

  type e = 
    | Connection_established of connection 
      (** Connection established with a server, it does not mean the server
          is a leader.  *)

    | Connection_closed 
      (** The connection with a server closed (brutal) *)

    | Request  of APb.client_request * result Lwt.u  
    
    | Response of APb.client_response * result Lwt.u  

    | Failure of string  
      (** System failure (unexpected) *)

    | Init
      (** Start the client loop *)

  let failure context () = Failure context

  let failure_lwt context () = Lwt.return (Failure context)

  let connection_closed (_, fd, _) () = 
    U.close fd >|= (fun () -> Connection_closed)  

  let connection_established fd () = 
    let connection = (
      Lwt_io.of_fd ~mode:Lwt_io.input fd, 
      fd, 
      Bytes.create 1024
    ) in 
    Connection_established connection

  let response client_response response_wakener () = 
    Response (client_response, response_wakener)
  
  let client_request client_request response_wakener () = 
    Request (client_request, response_wakener)

end 

module State = struct 

  type leader =
    | No
      (* No known leader *)

    | Potential   of int 
      (* One of the server indicated that this server should be a leader *)

    | Established of int * connection
      (* Successful on going connection with the leader *)

  type t = {
    configuration : Conf.t;
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
      | No          -> 
        failwith "Only potential leader can transition to established"
      | Potential i -> Established (i, fd)
    in 
    {state with leader}

  let next ({configuration; leader} as state) = 
    let nb_of_servers = 
      List.length (configuration.Conf.servers_ipc_configuration) 
    in 
    let next = match leader with
      | Established (i, _)  -> (i + 1) mod nb_of_servers
      | No                  -> 0
      | Potential i         -> (i + 1) mod nb_of_servers
    in 

    ({state with leader = Potential next}, next)

  let close_connection {leader; _} = 
    match leader with
    | Established (_, (_, fd, _)) -> U.close fd 
    | _ -> Lwt.return_unit 
    (* TODO: now that we have an Established connection which is closed
     * ... is it really `Established`*)
end 

type pending_request = APb.client_request * result Lwt.u 

type t = {
  mutable state : State.t; 
  configuration : Conf.t; 
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
let new_connection {state; _ } to_  = 
  match Conf.sockaddr_of_server_id `Client (state.State.configuration) to_ with
  | None -> 
    log_f ~level:Fatal 
          "Error getting address for server_id: %i" to_
    >|= Event.failure "Error gettting address"

  | Some ad ->
    let fd = U.socket U.PF_INET U.SOCK_STREAM 0 in
    Lwt.catch (fun () ->
      U.sleep 0.25
      >>=(fun () -> U.connect fd ad)
      >>=(fun () -> 
        log_f ~level:Notice 
              "Connection established successfully with server_id: %i" to_
       >|= Event.connection_established fd
      ) 
    ) (* with *) (fun exn ->

      log_f ~level:Warning 
            "Error connecting to server_id: %i, details: %s" 
            to_ (Printexc.to_string exn)
      >>= (fun () -> U.close fd >|= (fun () -> Event.Connection_closed))
    )


let decode_response bytes = 
  let decoder = Pbrt.Decoder.of_bytes bytes in 
  APb.decode_client_response decoder

(* Attempts to send the given [client_request] to the established connection 
 * with the leader. 
 *
 * In case of success [Response] event is returned. 
 * In case of communication failure, [Connection_closed] is returned. 
 *
 * -- Invariants --
 *
 * The function expects the leader to be established, if not [Failure] event
 * is returned. 
 *)
let handle_request ({state; _ } as t) client_request response_wakener = 
  let {State.leader; _ } = state in 

  match leader with
  | State.No | State.Potential _ -> 
     Event.failure_lwt ("Internal error, requests can only be " ^
                        "sent to established leader") ()

  | State.Established (server_id, connection) -> 
    
    let (ic, fd, buffer) = connection in 

    let bytes = 
      let encoder = Pbrt.Encoder.create () in
      APb.encode_client_request client_request encoder;
      Pbrt.Encoder.to_bytes encoder 
    in

    Lwt.catch (fun () ->
      Raft_utl_connection.write_msg_with_header fd bytes
      >>=(fun () -> 
        log_f ~level:Notice 
        "Request sent to server id: %i" server_id 
      ) 
      >>=(fun () -> 
        Raft_utl_connection.read_msg_with_header ic buffer 
      )
      >>=(fun (buffer', len) -> 
        log_f ~level:Notice 
              "Response received (size: %i)" len
        >>=(fun () -> 
          match decode_response (Bytes.sub buffer' 0 len) with
          | client_response -> begin  
            if buffer != buffer' 
            then begin 
              (* only need to update the connection if the buffer was 
               * resized *)
              let connection = (ic, fd, buffer') in 
              t.state <- {state with 
                State.leader = State.Established (server_id, connection); 
              }; 
            end; 

            let s = 
              Format.asprintf "%a" APb.pp_client_response client_response
            in  
            log_f ~level:Notice 
              "Client response decoded: %s" s

            >|= Event.response client_response response_wakener 
          end
          | exception exn ->
            log_f ~level:Error 
              "Error decoding client response, details: %s"
              (Printexc.to_string exn)
            >>= Event.connection_closed connection
        )
      )
    ) (* with *) (fun exn ->
      log_f ~level:Error "Error in IPC with RAFT server, details: %s" 
        (Printexc.to_string exn) 
      >>= Event.connection_closed connection
    )

let handle_response (_:t) client_response response_wakener = 

  match client_response with
  | APb.Add_log_result {APb.client_log_result_data; _ }  -> 
    Lwt.wakeup response_wakener client_log_result_data; 
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

let rec client_loop ({state; _} as t) e =

  match e with 
  | Event.Failure context -> (
    Printf.eprintf "Exiting: %s\n%!" context; 
    (exit 1 : unit); 
    Lwt.return_unit;
  )

  | Event.Connection_established fd ->
    t.state <- State.establish state fd;
    log_f ~level:Notice 
          "Connection established, %s" (State.string_of_state state) 
    >>=(fun () ->
      next_request t () >>= client_loop t 
    ) 

  | Event.Request (client_request, response_wakener) -> 
    handle_request t client_request response_wakener 
    >>=(fun e -> 
      log ~level:Notice "Request poped" 
      >>= (fun () -> client_loop t e)
    )  

  | Event.Response (client_response, response_wakener) ->  
    begin match handle_response t client_response response_wakener with
    | `Next_request -> 
       t.pending_request <- None;
       next_request t () >>= client_loop t 

    | `Not_a_leader leader_id -> 
      log_f ~level:Notice "Not a leader received, leader hint : %s"
        @@ (Ext.string_of_option string_of_int leader_id ) 
      >>=(fun () -> State.close_connection t.state) 
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

let make configuration = 
  let state = State.make configuration in 
  let request_stream, request_push  = Lwt_stream.create () in
  let t = {
    state; 
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

  let send {request_push; _} log = 
    let t, u = Lwt.wait () in
    let bytes = App.encode log in
    let request = Raft_com_pb.(Add_log_entry {
      client_log_id = unique_id (); 
      client_log_data = bytes;
    }) in 
    request_push (Some (request , u)); 
    (
      t
      >|=(function 
        | None -> None
        | Some bytes -> Some (App.decode bytes) 
      )
    )
end
