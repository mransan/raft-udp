open Lwt.Infix

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

  let connection_closed () = 
    Connection_closed 

  let connection_established fd () = 
    Connection_established fd

  let response_lwt client_response () = 
    Lwt.return (Response client_response)

end 

module State = struct 

  type leader =
    | No
      (* No known leader *)
    | Failed      of int 
      (* The connection just ended with this previous leader *)
    | Established of int * Lwt_unix.file_descr 
      (* Successful on going connection with the leader *)
    | Potential   of int 
      (* One of the server indicated that this server should be a 
         leader 
       *)

  type t = {
    configuration : Pb.configuration; 
    leader : leader;
  }

  let make () = {
    configuration = Conf.default_configuration ();
    leader = No;
  }

  let potential ({configuration; leader} as state) = 
    let nb_of_servers = List.length (configuration.Pb.servers_udp_configuration) in 
    let next = match leader with
      | Established _ -> failwith "No next leader when established"
      | Failed i      -> (i + 1) mod nb_of_servers
      | No            -> 0
      | Potential i   -> i 
    in 

    ({state with leader = Potential next}, next)

  let potential_of_leader_id state leader_id = 
    {state with leader = Potential leader_id}

  let fail ({leader; _ } as state)= 
    let leader = match leader with
      | Established (i, _)  -> Failed i 
      | Failed i      -> Failed i 
      | No            -> failwith "Cannot fail when no leader"
      | Potential i   -> Failed i
    in
    {state with leader}

  let establish ({leader; _ } as state) fd = 
    let leader = match leader with
      | Established _ 
      | Failed _ 
      | No -> failwith "Only potential leader can transition to established"
      | Potential i -> Established (i, fd)
    in 
    {state with leader}

  let fd {leader; _ } = 
    match leader with
    | Established (i, fd) -> Some fd 
    | _ -> None

end 

let new_connection state server_id = 
  match Conf.sockaddr_of_server_id `Client (state.State.configuration) server_id with
  | None -> 
    Lwt_io.eprintlf "Error getting address for server_id: %i" server_id
    >|= Event.failure "Error gettting address"
  | Some ad ->
    let fd = U.socket U.PF_INET U.SOCK_STREAM 0 in
    Lwt.catch (fun () ->
      U.connect fd ad
      >>=(fun () -> 
        Lwt_io.printlf "Connection established successfully with server_id: %i" 
          server_id
       >|= Event.connection_established fd
      ) 
    ) (* with *) (fun exn ->

      Lwt_io.printlf "Error connecting to server_id: %i" server_id
      >|= Event.connection_closed 
    )

let send_request {State.leader; _ } client_request = 
  let server_id, fd = match leader with 
    | State.Established (i, fd) -> (i, fd) 
    | _ -> failwith "Error, requests can only be sent to an establish leader"
  in 
  let encoder = Pbrt.Encoder.create () in
  Pb.encode_client_request client_request encoder;
  let buffer     = Pbrt.Encoder.to_bytes encoder in
  let buffer_len = Bytes.length buffer in

  Lwt.catch (fun () ->
    U.write fd buffer 0 buffer_len
    >>= (fun nb_byte_written ->
      if nb_byte_written <>  buffer_len
      then U.close fd >|= Event.connection_closed 
      else
        let buffer = Bytes.create 1024 in
        U.read fd buffer 0 1024
        >>= (fun bytes_read ->
          if bytes_read <> 0 && bytes_read <> 1024
          then
            let decoder = Pbrt.Decoder.of_bytes buffer in
            Event.response_lwt (Pb.decode_client_response decoder) ()
          else
            U.close fd >|= Event.connection_closed 
        )
    )
  ) (* with *) (fun exn ->
    Lwt_io.eprintlf "Error in IPC with RAFT server, details: %s" 
      (Printexc.to_string exn) 
      >>=(fun () -> 
        U.close fd >|= Event.connection_closed
      ) 
  )

let test_add_log_request = Pb.(Add_log {
  request_id = string_of_int (Unix.getpid());
    data = Bytes.of_string "Hi";
  })

let rec server_loop state = function
  
  | Event.Failure context -> (
    Printf.eprintf "Exiting: %s\n%!" context; 
    (exit 1 : unit); 
    Lwt.return_unit;
  )

  | Event.Connection_established fd ->
    let state = State.establish state fd in 
    send_request state test_add_log_request  
    >>= server_loop state

  | Event.Response Pb.Pong _ -> 
    server_loop state @@ Event.failure "Pong response" () 

  | Event.Response Pb.Add_log_success -> 
    Lwt_io.printlf "Success ..."
    >>=(fun () -> U.sleep 0.5)
    >>=(fun () ->
      send_request state test_add_log_request
      >>= server_loop state
    )
  
  | Event.Response Pb.Add_log_replication_failure -> 
    server_loop state @@ Event.failure "Replication failure" ()


  | Event.Response Pb.Add_log_not_a_leader {Pb.leader_id;} -> 
    Lwt_io.printlf "Not a leader received: %s"
      @@ (function | Some x -> string_of_int x | None -> "None") leader_id
    >>=(fun () -> 
      begin match State.fd state with
      | None -> server_loop state @@ Event.failure "No connection (invariant violation)"  ()
      | Some fd -> 
        U.close fd 
        >>=(fun () ->
          let state, leader_id = match leader_id with
            | None -> State.fail state |> State.potential 
            | Some leader_id -> 
              State.potential_of_leader_id state leader_id, leader_id 
          in

          new_connection state leader_id 
          >>= server_loop state 
        ) 
      end
    )
  
  | Event.Connection_closed ->
    let state, leader_id = State.fail state |> State.potential in  
    U.sleep 0.25
    >>=(fun () -> new_connection state leader_id)
    >>= server_loop state 

  | Event.Init ->
    let state, leader_id = State.potential state in 
    new_connection state leader_id
    >>= server_loop state 

let () =
  Lwt_main.run (server_loop (State.make ()) Event.Init)
