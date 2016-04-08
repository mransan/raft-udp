
module Raft = Raft_pb
module Udp  = Raft_udp_pb

open Lwt.Infix

let default_configuration () = Udp.({
  raft_configuration = Raft.({
    nb_of_server = 3;
    election_timeout = 10.;
    election_timeout_range = 1.;
    hearbeat_timeout = 5.;
  });
  servers_udp_configuration = [
    {raft_id = 0; inet4_address = "127.0.0.1"; port = 31000; };
    {raft_id = 1; inet4_address = "127.0.0.1"; port = 31001; };
    {raft_id = 2; inet4_address = "127.0.0.1"; port = 31002; };
  ];
})

let new_election_timeout ?timeout {Udp.raft_configuration; _ } = 
  let timeout = match timeout with
    | Some timeout -> timeout
    | None -> 
      let {Raft.election_timeout; _ } =  raft_configuration in 
      election_timeout
  in 
  let range  = raft_configuration.Raft.election_timeout_range in 
  timeout +. (Random.float range -. (range /. 2.))

let timeout_of_wait_rpc configuration {Raft_pb.timeout; timeout_type} = 
  match timeout_type with
  | Raft.Heartbeat -> timeout 
  | Raft.New_leader_election -> new_election_timeout ~timeout configuration 

let sockaddr_of_server_config {Udp.inet4_address; port} =
  Unix.ADDR_INET (Unix.inet_addr_of_string inet4_address, port)

let sockaddr_of_server_id configuration server_id =
  let is_server {Udp.raft_id; _ } =
    raft_id = server_id
  in
  match List.find is_server configuration.Udp.servers_udp_configuration with
  | server_config -> Some (sockaddr_of_server_config server_config)
  | exception Not_found -> None

let receive_stream configuration server_id =
  match sockaddr_of_server_id configuration server_id with
  | None ->
    failwith @@ Printf.sprintf "Invalid configuration, server(%i) not found" server_id
  | Some ad ->

    let module U = Lwt_unix in
    let fd = U.socket U.PF_INET U.SOCK_DGRAM 0 in
    U.bind fd ad;

    let buffer_size = 1024 * 1024 in
    let buffer = Bytes.create buffer_size in

    let rec receive_loop () =
      U.recvfrom fd buffer 0 buffer_size []
      >|= (fun (nb_of_bytes_received, _) ->
        let decoder = Pbrt.Decoder.of_bytes buffer in
        `Message (Raft.decode_message decoder)
      )
    in
    receive_loop

let get_send_msg_f configuration =

  let module U = Lwt_unix in

  let server_addresses = List.map (fun ({Udp.raft_id} as server_config) ->
    (raft_id, sockaddr_of_server_config server_config)
  ) configuration.Udp.servers_udp_configuration in

  let fd = U.socket U.PF_INET U.SOCK_DGRAM 0 in

  (fun msg server_id ->
    match List.assq server_id server_addresses with
    | ad -> (
      let encoder = Pbrt.Encoder.create () in
      Raft.encode_message msg encoder;
      let buffer  = Pbrt.Encoder.to_bytes encoder in
      let buffer_size = Bytes.length buffer in

      U.sendto fd buffer 0 buffer_size [] ad
      >|= (fun nb_of_bytes_written ->
        if nb_of_bytes_written = buffer_size
        then `Success
        else `Failure
      )
    )
    | exception Not_found -> Lwt.return `Failure
  )

(* -- Server -- *)

let run_server configuration id =
  let now   = Unix.gettimeofday () in

  let now () =
    Unix.gettimeofday () -. now
  in

  let send_msg = get_send_msg_f configuration in

  let state = Raft_helper.Follower.create
    ~configuration:configuration.Udp.raft_configuration ~id () in

  let action = Raft_helper.Follow_up_action.default state (now ()) in

  let hearbeat_timeout = configuration.Udp.raft_configuration.Raft.hearbeat_timeout in 

  Format.printf "initial state: %a\n%!"  Raft.pp_state state;
  Format.printf "initial action: %a\n%!" Raft.pp_follow_up_action action;

  let receive_loop = receive_stream configuration id in
  
  let send_all f state = 
    let requests = f state in 
    Lwt_list.fold_left_s (fun state (msg, receiver_id) -> 
      send_msg msg receiver_id
      >|= (fun _ -> 
        match state.Raft.role with
        | Raft.Leader leader_state -> (
          let configuration = configuration.Udp.raft_configuration in 
          let leader_state  = fst @@ Raft_helper.Leader.update_receiver_deadline 
            ~server_id:receiver_id ~now:(now()) ~configuration leader_state
          in 
          {state with Raft.role = Raft.Leader leader_state;} 
        ) 
        | _ -> state 
      )
    ) state requests
  in

  let send_all_append_entries state = 
    send_all Raft_logic.Message.append_entries_request_for_all state
  in
  
  let send_all_request_votes state = 
    send_all Raft_logic.Message.request_vite_for_all state
  in

  let rec loop state timeout timeout_type =
    Lwt.catch (fun () ->
      Printf.printf "Waiting on RPC for timeout: %f\n%!" timeout; 
      Lwt_unix.with_timeout timeout receive_loop
    ) (function
        | Lwt_unix.Timeout -> Lwt.return `Timeout
        | _                -> Lwt.return `Failure
    )
    >>=(function
      | `Message msg -> (
        Format.printf "--------------------------------------------\n";
        Format.printf "Message received: %a\n%!" Raft.pp_message msg;
        Format.printf "Current state: %a\n%!" Raft.pp_state state;
        let state, response, action = Raft_logic.Message.handle_message state msg (now ()) in
        Format.printf "New state: %a\n%!" Raft.pp_state state;
        Format.printf "action: %a\n%!" Raft.pp_follow_up_action action;
        let send_t = 
          match response with
          | None -> (
            Format.printf "--------------------------------------------\n";
            Lwt.return_unit
          )
          | Some (response_msg, receiver_id) ->
            Format.printf "Response sent(%i): %a\n%!" receiver_id Raft.pp_message response_msg;
            Format.printf "--------------------------------------------\n";
            send_msg response_msg receiver_id
            >|= ignore 
        in
        send_t
        >>=(fun () -> 
          match action with
          | Raft.Wait_for_rpc ({Raft.timeout_type; } as wait_for_rpc) -> 
            loop state (timeout_of_wait_rpc configuration wait_for_rpc) timeout_type 
          | Raft.Act_as_new_leader -> (
            send_all_append_entries state 
            >>= (fun state -> loop state hearbeat_timeout Raft.Heartbeat) 
          )
        ) 

      )
      | `Timeout -> (
        Printf.printf "Timeout ... %f \n%!" (now ());
        match timeout_type with
        | Raft.Heartbeat -> (
          send_all_append_entries state 
          >>= (fun state -> loop state hearbeat_timeout Raft.Heartbeat)
        )
        | Raft.New_leader_election -> (
          let state = Raft_helper.Candidate.become ~now:(now()) state in 
          send_all_request_votes state
          >>= (fun state -> 
            loop state (new_election_timeout configuration) Raft.New_leader_election
          )
        )
      )
      | `Failure -> (
        Lwt_io.eprintf "System failure...\n%!"
      )
    )
  in

  let timeout = new_election_timeout configuration in
  Lwt_main.run (loop state timeout Raft.New_leader_election)

let run_client configuration to_server_id =
  let module U = Lwt_unix in

  let send_msg = get_send_msg_f configuration in

  let msg = Raft.(Request_vote_request {
    candidate_term = 1;
    candidate_id = 1;
    candidate_last_log_index = 10;
    candidate_last_log_term  = 10;
  }) in

  Lwt_main.run (
    send_msg msg to_server_id
    >>=(function
      |`Success -> Lwt_io.printf "Success \n"
      |`Failure -> Lwt_io.printf "Failure \n"
    )
  )

let () =
  let configuration = default_configuration () in

  let mode = ref `Client in
  let id   = ref (-1) in

  let mode_spec = Arg.Symbol (["client"; "server"], function
    | "client" -> mode := `Client
    | "server" -> mode := `Server
    | _ -> ()
  ) in

  let id_spec = Arg.Symbol (["0";"1";"2"], fun s ->
    id := int_of_string s
  ) in

  Arg.parse [
    ("-m", mode_spec, " : mode");
    ("--id", id_spec , " : server raft id");
  ] (fun _ -> ()) "test.ml";

  match !mode with
  | `Server -> run_server configuration !id
  | `Client -> run_client configuration !id
