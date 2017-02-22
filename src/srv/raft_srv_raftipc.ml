open Lwt.Infix 
open !Lwt_log_core

module Counter      = Raft_utl_counter

module APb          = Raft_com_pb
module Server_stats = Raft_srv_serverstats
module Log          = Raft_srv_log 
module Log_record   = Raft_srv_logrecord
module Conf         = Raft_com_conf

module RTypes = Raft_types
module RLog   = Raft_log
module RPb    = Raft_pb 
module RConv  = Raft_pb_conv

type client_request   = Raft_com_pb.client_request * Raft_srv_clientipc.handle
type client_response  = Raft_com_pb.client_response * Raft_srv_clientipc.handle 
type client_responses = client_response list 
type app_requests = Raft_com_pb.app_request list 
type app_response = Raft_com_pb.app_response 

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

  type t = client_request StringMap.t
  
  let add t request_id client_request = 
    StringMap.add request_id client_request t
  
  let get_and_remove t request_id =
    match StringMap.find request_id t with
    | client_request ->
      let t = StringMap.remove request_id t  in
      (t, Some client_request)
    | exception Not_found ->
      (t, None)

  let empty = StringMap.empty 

end (* Pending_requests *)

(* All the outgoing RAFT messages are sent in a dedicated
 * concurrent thread [state.outgoing_message_processing]. The 
 * RAFT protocol nevers requires the sender of a RAFT message to block and 
 * wait for any responses. (In fact that response might never come).
 *
 * In order to send those message concurrently, an [Lwt_stream] is used to 
 * decouple the threads which wants to compute the outgoing messages from 
 * the one which actually sends them.  
 * 
 * For the caller of this API sending a response is simply pushing the 
 * response to the stream (immediate). The Lwt scheduler will then 
 * pick it up asynchronously by scheduling [state.outgoing_message_processing]
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

  let server_addresses = List.map (fun ({Conf.raft_id; _ } as server_config) ->
    let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_DGRAM 0 in
    (raft_id, (Conf.sockaddr_of_server_config `Raft server_config, fd))
  ) configuration.Conf.servers_ipc_configuration in

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
  raft_state: RTypes.state; 
  connection_state: connection_state; 
  log_record_handle : Log_record.t;
}

type result = (state * client_responses * app_requests) 

let handle_added_logs logger stats 
                          compaction_handle added_logs = 

  match added_logs with
  | [] -> Lwt.return_unit
  | _ ->
    let _  = stats in 
      (* TODO probably need to collect some stats *)

    let log_entries = List.map RConv.log_entry_to_pb added_logs in 

    (* The RAFT protocol dictates that all committed log entries must be stored
     * permanently on DISK. This way, if a server crashes it can recover.  *)
    Log_record.add_logs logger added_logs compaction_handle 

let handle_committed_logs logger stats 
                          compaction_handle committed_logs () = 

  match committed_logs with
  | [] -> Lwt.return []
  | _ ->
    let _  = stats in 
      (* TODO probably need to collect some stats *)

    let log_entries = List.map RConv.log_entry_to_pb committed_logs in 

    let app_requests = [APb.(Add_log_entries {log_entries})] in  
    
    Log_record.set_committed logger committed_logs compaction_handle 
    >|=(fun () -> app_requests)

let send_raft_messages ~logger ~stats id connection_state messages  = 
  let {push_outgoing_message; _ } = connection_state in 

  Lwt_list.map_p (fun ((msg, server_id) ) ->
    let msg = RConv.message_to_pb msg in 
    let msg_to_send  = (msg, server_id) in 
    Server_stats.tick_raft_msg_send stats; 
    Raft_srv_log.print_msg_to_send logger section id msg server_id 
    >|= (fun () -> push_outgoing_message (Some msg_to_send))
  ) messages
  >|= ignore

let process_result logger stats state result =  
  let {log_record_handle; connection_state; _ } = state in 

  let {
    Raft_logic.state = raft_state; 
    messages_to_send = outgoing_messages; 
    committed_logs;
    leader_change = _;  
      (* TODO handle leader_change by replying to cleaning up the client
       * pending requests *)
    added_logs; 
  }  = result in 

  handle_added_logs
        logger state log_record_handle added_logs 
  >>= handle_committed_logs 
        logger stats log_record_handle committed_logs
  >>=(fun app_requests -> 
    send_raft_messages 
        ~logger ~stats (raft_state.RTypes.server_id) 
        connection_state outgoing_messages
    >|= (fun () -> 
      ({state with raft_state; connection_state}, [] , app_requests)
    ) 
  )

let handle_raft_message ~logger ~stats ~now state msg = 
  let {raft_state; _ } = state in 

  log ~logger ~level:Notice ~section "Raft Message Received"
  >>=(fun () -> Log.print_state logger section raft_state)
  >>=(fun () -> 
    Log.print_msg_received logger section msg raft_state.RTypes.server_id)
  >>=(fun () ->
    Server_stats.tick_raft_msg_recv stats;

    let msg = RConv.message_of_pb msg in
    let perf = Server_stats.msg_processing stats in 
    let result = 
      Counter.Perf.f3 perf Raft_logic.handle_message raft_state msg now 
    in

    process_result logger stats state result  
  )

let handle_timeout ~logger ~stats ~now state timeout_type = 
  let { raft_state; _} = state in 
  begin match timeout_type with
    | RTypes.Heartbeat -> (
      Server_stats.tick_heartbeat stats;
      log ~logger ~section ~level:Notice "Heartbeat timeout" 
      >|= (fun () ->
        Counter.Perf.f2 (Server_stats.hb_processing stats)
          Raft_logic.handle_heartbeat_timeout raft_state now
      )
    )

    | RTypes.New_leader_election -> (
      Printf.printf "NEW LEADER ELECTION [%2i] \n%!" 
            raft_state.RTypes.server_id;
      log ~logger ~level:Notice ~section "Leader Election timeout"
      >|= (fun () ->
        Raft_logic.handle_new_election_timeout raft_state now
      ))
  end
  >>=(fun result -> 

    process_result logger stats state result  
  ) 

let handle_client_requests ~logger ~stats ~now  state client_requests = 

  let _ = logger and _ = stats and _ = now in 
  let {connection_state; raft_state; _ } = state in 
  let {pending_requests; _ } = connection_state in 

  let pending_requests, datas = List.fold_left (fun acc client_request ->
    let (pending_requests, datas) = acc in 
    let (client_request_msg, _ (*handle*)) = client_request in 
    match client_request_msg with
    | APb.Add_log_entry {APb.client_log_id; client_log_data}  -> 
      let pending_requests = 
        Pending_requests.add pending_requests client_log_id client_request 
      in 
      (pending_requests, (client_log_data, client_log_id) :: datas) 
  ) (pending_requests, []) client_requests in

  let new_log_response  = 
    Raft_logic.handle_add_log_entries raft_state datas now 
  in 

  begin match new_log_response with
  | Raft_logic.Delay
  | Raft_logic.Forward_to_leader _ -> 
    log ~logger ~level:Notice ~section "New logs rejected since not a leader"
    >|= (fun () ->

      let client_responses = List.map (fun (_, handle) -> 
        let client_response_msg = APb.(Add_log_not_a_leader {
          leader_id = RTypes.current_leader raft_state
        }) in 

        (client_response_msg, handle)
      ) client_requests in

      (state, client_responses, [])
    )

  | Raft_logic.Appended result -> 
    log_f ~logger ~level:Notice ~section 
          "Log Added (log size: %i) (nb logs: %i)" 
           raft_state.RTypes.log.RLog.log_size (List.length datas)  
    >>= (fun () ->

      let state = {state with 
        connection_state = {connection_state with pending_requests; }
      } in 

      process_result logger stats state result  
    )
  end
      
let process_app_result logger acc result =  

  let (pending_requests, client_responses)  = acc in 

  let {APb.index = _; id; APb.result_data} = result  in 
  
  let (
    pending_requests, 
    client_request
  ) = Pending_requests.get_and_remove pending_requests id in 

  match client_request with
  | None -> 
    (* This is ok that there is no client request associated with a log id, 
     * this is most likely the case we are in a follower. However in the 
     * leader, this would be an error. (until we support the 
     * proper handling a client disconnection) *)
    log_f ~logger ~level:Notice ~section 
          "Could not find pending request after validation for id: %s" id 
    >|=(fun () -> (pending_requests, client_responses)) 

  | Some (APb.Add_log_entry _, handle) ->
    let client_response_msg = APb.Add_log_result {
      APb.client_log_id = id; 
      APb.client_log_result_data = result_data; 
    } in 

    let client_responses = (client_response_msg, handle)::client_responses in

    log_f ~logger ~level:Notice ~section 
          "Matching client request found for app response, id: %s"
          id 
    >|=(fun () -> (pending_requests, client_responses)) 

let handle_app_response ~logger ~stats ~now state app_response = 

  let _ = stats and _ = now in 

  let {connection_state; _ } = state in 
  let {pending_requests; _} = connection_state in 

  match app_response with
  | APb.Add_log_results {APb.results} -> 

    Lwt_list.fold_left_s 
        (process_app_result logger) 
        (pending_requests, []) 
        results 
    >|=(fun (pending_requests, client_responses) ->
      let state = {state with 
        connection_state = {connection_state with pending_requests; }
      } in 
      (state, client_responses, []) 
    )
