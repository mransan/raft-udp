open Lwt_log_core
open Lwt.Infix 

module Pb           = Raft_udp_pb
module Server_stats = Raft_udp_serverstats
module Counter      = Raft_udp_counter
module Client_ipc   = Raft_udp_clientipc 
module Log          = Raft_udp_log 
module Log_record   = Raft_udp_logrecord
module Conf         = Raft_udp_conf

module RState = Raft_state
module RPb = Raft_pb 

type raft_message     = RPb.message * int 
type raft_messages    = raft_message list 
type client_request   = Raft_udp_pb.client_request * Raft_udp_clientipc.handle
type client_response  = Raft_udp_pb.client_response * Raft_udp_clientipc.handle 
type client_responses = client_response list 
type notifications = RPb.notification list

let section = Section.make (Printf.sprintf "%10s" "RaftIPC")

type event = 
  | Raft_message of Raft_pb.message
  | Failure 

type next_raft_message_f = unit -> event Lwt.t  

let get_next_raft_message_f_for_server configuration server_id =
  
  match Conf.sockaddr_of_server_id `Raft configuration server_id with
  | None ->
    (fun () -> Lwt.return Failure)

  | Some ad ->

    let module U = Lwt_unix in
    let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_DGRAM 0 in
    Lwt_unix.bind fd ad;

    let buffer_size = (1024 * 1024)  in
    let buffer = Bytes.create buffer_size in

    fun () ->
      Lwt_unix.recvfrom fd buffer 0 buffer_size []
      >|= (fun (nb_of_bytes_received, _) ->
        let decoder = Pbrt.Decoder.of_bytes (Bytes.sub buffer 0 nb_of_bytes_received) in
        Raft_message (RPb.decode_message decoder)
      )
  
module StringMap = Map.Make(struct
  type t = string
  let compare (x:string) (y:string) = Pervasives.compare x y
end)

module Pending_requests = struct 

  type t = Client_ipc.handle StringMap.t
  
  let add t request_id handle  = 
    StringMap.add request_id handle t
  
  let get t request_id =
    match StringMap.find request_id t with
    | handle ->
      let t = StringMap.remove request_id t  in
      (t, Some handle)
    | exception Not_found ->
      (t, None)

  let empty = StringMap.empty 

end (* Pending_requests *)

(*
 * All the outgoing RAFT messages are sent in a dedicated
 * concurrent thread [state.outgoing_message_processing]. The 
 * RAFT protocol nevers requires the sender of a RAFT message to block and wait for any 
 * responses. (In fact that response might never come).
 *
 * In order to send those message concurrently, an [Lwt_stream] is used to decouple
 * the threads which wants to compute the outgoing messages from the one which 
 * actually sends them.  
 * 
 * For the caller of this API sending a response is simply pushing the 
 * response to the stream (immediate). The Lwt scheduler will then 
 * pick it up asynchronously by scheduling [state.outgoing_message_processing]
 *
 *)
  
type connection_state = {
  pending_requests : Pending_requests.t;
    (* Keeps track of pending request from client *)
  outgoing_message_processing : unit Lwt.t; 
    (* Threads which sends the outgoing messages *)
  push_outgoing_message : (RPb.message * int) option -> unit;
    (* Function to push an outgoing message in the stream *)
}

let initialize configuration = 

  let server_addresses = List.map (fun ({Pb.raft_id} as server_config) ->
    let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_DGRAM 0 in
    (raft_id, (Conf.sockaddr_of_server_config `Raft server_config, fd))
  ) configuration.Pb.servers_udp_configuration in

  let (
    outgoing_message_stream, 
    push_outgoing_message
  ) = Lwt_stream.create () in 

  let outgoing_message_processing = Lwt_stream.iter_s (fun (msg, server_id) ->
    (* IMPORTANT : DO NOT USE Lwt_stream.iter_p, this caused 
     * large increase in memory usage. It looks like the memory
     * is not garbage collected.
     *)
    match List.assq server_id server_addresses with
    | (ad, fd) -> (
      let encoder = Pbrt.Encoder.create () in
      RPb.encode_message msg encoder;
      let buffer  = Pbrt.Encoder.to_bytes encoder in
      let buffer_size = Bytes.length buffer in

      let rec sendto from remaining =
        Lwt_unix.sendto fd buffer 0 buffer_size [] ad
        >>= (function 
          | 0 -> Lwt.return_unit
            (* We ignore the failure here, the RAFT protocol
             * supports message not being delivered and will
             * ensure that it will recover. 
             *)
          | nb_bytes when nb_bytes = remaining ->
            Lwt.return_unit
            (* All good message is delivered
             *)
          | nb_bytes -> 
            sendto (from + nb_bytes) (remaining - nb_bytes)
        )
      in
      sendto 0 buffer_size
    )
    | exception Not_found -> 
      Lwt.fail_with @@ Printf.sprintf "Address not found for server %i" server_id
  ) outgoing_message_stream
  in  

  {
    pending_requests = Pending_requests.empty; 
    outgoing_message_processing;
    push_outgoing_message;
  }
    
type state = {
  raft_state: RPb.state; 
  connection_state: connection_state; 
  log_record_handle : Log_record.t;
}

let handle_notifications logger connection_state compaction_handle notifications = 

  (* 
   * Go over each notifications and compute the client response by looking
   * up the map of pending request. 
   *)
  let connection_state, client_responses = List.fold_left (fun acc notification ->

    match notification with
    | RPb.Committed_data {RPb.rev_log_entries} -> 
      let ids = List.map (fun ({RPb.id; _ }:RPb.log_entry) -> id) rev_log_entries in 
      List.fold_left (fun (connection_state, client_responses) id -> 
        let {pending_requests; _ }   = connection_state in 
        let pending_requests, handle = Pending_requests.get pending_requests id in 
        let connection_state = {connection_state with pending_requests } in 
        match handle with
        | None -> 
          (connection_state, client_responses) 
        | Some handle -> 
          (connection_state, ((Pb.Add_log_success, handle)::client_responses))
      ) acc ids 

    | RPb.New_leader _
    | RPb.No_leader  -> 
      (* TODO need to go through all the pending requests. 
       *)
      acc
    ) (connection_state, []) notifications
  in

  (* 
   * The RAFT protocol dictates that all commited log entries must be stored
   * permanently on DISK. This way, if a server crashes it can recover. 
   *)
  Lwt_list.iter_s (function
    | RPb.Committed_data {RPb.rev_log_entries} -> 
      Log_record.append_commited_data logger rev_log_entries compaction_handle 
    | _ -> Lwt.return_unit
  ) notifications
  >|=(fun () -> (connection_state, client_responses))

let send_raft_messages ~logger ~stats id connection_state messages  = 
  let {push_outgoing_message; } = connection_state in 

  Lwt_list.map_p (fun ((msg, server_id) as msg_to_send) ->
    Server_stats.tick_raft_msg_send stats; 
    Raft_udp_log.print_msg_to_send logger section id msg server_id 
    >|= (fun () -> push_outgoing_message (Some msg_to_send))
  ) messages
  >|= ignore

let handle_raft_message ~logger ~stats ~now state msg = 
  let { 
    raft_state; 
    connection_state; 
    log_record_handle; 
  } = state in 

  log ~logger ~level:Notice ~section "Raft Message Received"
  >>=(fun () -> Log.print_state logger section raft_state)
  >>=(fun () -> Log.print_msg_received logger section msg raft_state.RPb.id)
  >>=(fun () ->
    Server_stats.tick_raft_msg_recv stats;

    let perf = Server_stats.msg_processing stats in 
    let ret  = Counter.Perf.f3 perf Raft_logic.handle_message raft_state msg now in

    let (raft_state, outgoing_messages, notifications) = ret in 

    handle_notifications logger  connection_state log_record_handle notifications 
    >>=(fun (connection_state, client_responses) ->
      send_raft_messages ~logger ~stats (raft_state.RPb.id) connection_state outgoing_messages
      >|= (fun () -> 
        ({state with raft_state; connection_state}, client_responses)
      ) 
    )
  )

let handle_timeout ~logger ~stats ~now state timeout_type = 
  let { raft_state; connection_state; log_record_handle ; } = state in 
  begin match timeout_type with
  | RPb.Heartbeat -> (
    Server_stats.tick_heartbeat stats;
    log ~logger ~section ~level:Notice "Heartbeat timeout" 
    >|= (fun () ->
      Counter.Perf.f2 (Server_stats.hb_processing stats)
        Raft_logic.handle_heartbeat_timeout raft_state now
    )
    >|= (fun (raft_state, outgoing_messages) -> (raft_state, outgoing_messages, []))
  )

  | RPb.New_leader_election -> (
    print_endline "NEW LEADER ELECTION%!";
    log ~logger ~level:Notice ~section "Leader Election timeout"
    >|= (fun () ->
      Raft_logic.handle_new_election_timeout raft_state now
    ))
  end

  >>=(fun (raft_state, outgoing_messages, notifications) ->

    handle_notifications logger  connection_state log_record_handle notifications 
    >>=(fun (connection_state, client_responses) ->
      send_raft_messages ~logger ~stats (raft_state.RPb.id) connection_state outgoing_messages
      >|= (fun () -> 
        ({state with raft_state; connection_state}, client_responses)
      ) 
    )
  ) 

let handle_client_request ~logger ~stats ~now  state (client_request, handle) = 

  let { raft_state; connection_state; _ } = state in 

  match client_request with

  | Pb.Add_log {Pb.request_id; data;} -> 

    let new_log_response  = 
      let datas = [(data, request_id)] in 
      Raft_logic.handle_add_log_entries raft_state datas now 
    in 

    begin match new_log_response with
    | Raft_logic.Delay
    | Raft_logic.Forward_to_leader _ -> 
      log ~logger ~level:Notice ~section "Log Rejected "
      >|= (fun () ->

        let client_response = Pb.(Add_log_not_a_leader {
          leader_id = RState.current_leader raft_state;
        }) in 

        let client_response = (client_response, handle) in 
        (state, [client_response])
      )

    | Raft_logic.Appended (raft_state, outgoing_messages) -> 
      log_f ~logger ~level:Notice ~section "Log Added (log size: %i)" raft_state.RPb.log_size 
      >>= (fun () ->
        let {pending_requests; _ } = connection_state in 
        let connection_state = {connection_state with
          pending_requests = Pending_requests.add pending_requests request_id handle;
        } in 

        send_raft_messages ~logger ~stats (raft_state.RPb.id) connection_state outgoing_messages
        >|= (fun () -> 
          ({state with raft_state; connection_state}, [])
        ) 
      )
    end