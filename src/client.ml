open Lwt.Infix
open Lwt_log_core 

module Conf = Raft_udp_conf
module U    = Lwt_unix
module Pb   = Raft_udp_pb

module Event = struct 

  type e = 
    | Connection_established of Lwt_unix.file_descr
      (* Connection established with a server, it does not mean the server
       * is a leader. 
       *)
    | Connection_closed 
      (* The connection with a server closed (brutal) 
       *)
    | Response of Pb.client_response 
      (* The leader replied a response *)
    | Failure of string  
      (* System failure (unexpected) *)
    | Init
      (* Start the client loop *)

  let failure context () = Failure context

  let failure_lwt context () = Lwt.return (Failure context)

  let connection_closed ?fd () = 
    match fd with 
    | None -> Lwt.return Connection_closed 
    | Some fd -> U.close fd >|= (fun () -> Connection_closed)  

  let connection_established fd () = 
    Connection_established fd

  let response_lwt client_response () = 
    Lwt.return (Response client_response)

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
    configuration : Pb.configuration; 
    leader : leader;
  }

  let string_of_state {leader; _ } = 
    match leader with 
    | No -> "No"
    | Potential i -> Printf.sprintf "Potential(%i)" i
    | Established (i, _) -> Printf.sprintf "Potential(%i)" i 

  let make () = {
    configuration = Conf.default_configuration ();
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
    let nb_of_servers = List.length (configuration.Pb.servers_udp_configuration) in 
    let next = match leader with
      | Established (i, _)  -> (i + 1) mod nb_of_servers
      | No                  -> 0
      | Potential i         -> (i + 1) mod nb_of_servers
    in 

    ({state with leader = Potential next}, next)

  let fd {leader; _ } = 
    match leader with
    | Established (i, fd) -> Some fd 
    | _ -> None

end 

(* Attempts to establish a TCP connection to the give server. 
 *
 * In case of success the returned event will [Connection_established] 
 * while on failure it will be [Connection_closed]. 
 *) 
let new_connection ~to_ state = 
  match Conf.sockaddr_of_server_id `Client (state.State.configuration) to_ with
  | None -> 
    Lwt_io.eprintlf "Error getting address for server_id: %i" to_
    >|= Event.failure "Error gettting address"

  | Some ad ->
    let fd = U.socket U.PF_INET U.SOCK_STREAM 0 in
    Lwt.catch (fun () ->
      U.connect fd ad
      >>=(fun () -> 
        Lwt_io.printlf "Connection established successfully with server_id: %i" to_
       >|= Event.connection_established fd
      ) 
    ) (* with *) (fun exn ->

      Lwt_io.printlf "Error connecting to server_id: %i" to_
      >>= Event.connection_closed 
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
let send_request logger ({State.leader; _ } as state) client_request = 
  match leader with
  | State.No | State.Potential _ -> 
     Lwt.return (Event.Failure "Internal error, requests can only be sent to established leader") 
  | State.Established (server_id, fd) -> 
    let encoder = Pbrt.Encoder.create () in
    Pb.encode_client_request client_request encoder;
    let buffer     = Pbrt.Encoder.to_bytes encoder in
    let buffer_len = Bytes.length buffer in

    Lwt.catch (fun () ->
      U.write fd buffer 0 buffer_len
      >>= (fun nb_byte_written ->
        if nb_byte_written <>  buffer_len
        then Event.connection_closed ~fd () 
        else
          log_f ~logger ~level:Notice "Request sent to state: %s" (State.string_of_state state) 
          >>=(fun () -> 
            let buffer = Bytes.create 1024 in
            U.read fd buffer 0 1024
            >>= (fun bytes_read ->
              log_f ~logger ~level:Notice "Response received (size: %i)" bytes_read
              >>=(fun () -> 
                if bytes_read <> 0 && bytes_read <> 1024
                then
                  let decoder = Pbrt.Decoder.of_bytes (Bytes.sub buffer 0 bytes_read) in
                  Event.response_lwt (Pb.decode_client_response decoder) ()
                else
                  Event.connection_closed ~fd () 
              )
            )
          )
      )
    ) (* with *) (fun exn ->
      Lwt_io.eprintlf "Error in IPC with RAFT server, details: %s" 
        (Printexc.to_string exn) 
        >>= Event.connection_closed ~fd
    )


let unique_request_id = ref 0 

let test_add_log_request () = 
  incr unique_request_id; 
  Pb.(Add_log {
    request_id = Printf.sprintf "%i|%i" (Unix.getpid()) !unique_request_id;
    data = Bytes.of_string (String.make (Random.int 10) 'a');
  })

let rec server_loop logger state count e =

  match e with 
  | Event.Failure context -> (
    Printf.eprintf "Exiting: %s\n%!" context; 
    (exit 1 : unit); 
    Lwt.return_unit;
  )

  | Event.Connection_established fd ->
    let state = State.establish state fd in 
    log_f ~logger ~level:Notice "Connection established, %s" (State.string_of_state state) 
    >>=(fun () ->
      send_request logger state @@ test_add_log_request () 
      >>= server_loop logger state count
    ) 

  | Event.Response Pb.Add_log_success -> 
    begin if (count mod 1000) = 0
    then 
      Lwt_io.printlf "Success ... [%i]" count 
    else 
      Lwt.return_unit
    end
    >>=(fun () -> U.sleep 0.0)
    >>=(fun () ->
      send_request logger state @@ test_add_log_request ()
      >>= server_loop logger state (count + 1)
    ) 
  
  | Event.Response Pb.Add_log_validation_failure -> 
    server_loop logger state count @@ Event.failure "Validation failure" ()

  | Event.Response Pb.Add_log_not_a_leader {Pb.leader_id;} -> 
    Lwt_io.printlf "Not a leader received: %s"
      @@ (function | Some x -> string_of_int x | None -> "None") leader_id
    >>=(fun () -> 
      begin match State.fd state with
      | None -> server_loop logger state count @@ Event.failure "No connection (invariant violation)"  ()
      | Some fd -> 
        U.close fd 
        >>=(fun () ->

          let state, leader_id = match leader_id with
            | None -> State.next state
            | Some leader_id -> 
              State.potential state leader_id, leader_id 
          in

          new_connection ~to_:leader_id state 
          >>= server_loop logger state count 
        ) 
      end
    )
  
  | Event.Connection_closed ->
    let state, leader_id = State.next state in  
    U.sleep 0.25
    >>=(fun () -> new_connection ~to_:leader_id state )
    >>= server_loop logger state count 

  | Event.Init ->
    let state, leader_id = State.next state in 
    new_connection ~to_:leader_id state 
    >>= server_loop logger state count 

let main log () = 
  begin 
    if log 
    then 
      let file_name = Printf.sprintf "client%i.log" (Unix.getpid ()) in 
      let template  = "$(date).$(milliseconds) [$(level)] [$(section)] : $(message)" in
      Lwt_log.file ~mode:`Truncate ~template ~file_name ()
    else 
      Lwt.return Lwt_log_core.null 
  end 
  >>=(fun logger -> server_loop logger (State.make ()) 0 Event.Init)

let () =
  let log = ref false in 
  let log_spec = Arg.Set log  in
  
  Arg.parse [
    ("--log", log_spec, " : enable logging");
  ] (fun _ -> ()) "client.ml";

  Random.self_init ();
  Lwt_main.run (main !log ())
