open Lwt.Infix 
open !Lwt_log_core

module Counter      = Raft_utl_counter

module UPb          = Raft_udp_pb
module APb          = Raft_app_pb
module Server_stats = Raft_srv_serverstats
module Log          = Raft_srv_log 
module Log_record   = Raft_srv_logrecord
module Conf         = Raft_udp_conf

module RTypes = Raft_types
module RLog   = Raft_log
module RPb    = Raft_pb 

type client_request   = Raft_app_pb.client_request * Raft_srv_clientipc.handle
type client_response  = Raft_app_pb.client_response * Raft_srv_clientipc.handle 
type client_responses = client_response list 
type app_requests = Raft_app_pb.app_request list 
type app_response = Raft_app_pb.app_response 

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

  let server_addresses = List.map (fun ({UPb.raft_id; _ } as server_config) ->
    let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_DGRAM 0 in
    (raft_id, (Conf.sockaddr_of_server_config `Raft server_config, fd))
  ) configuration.UPb.servers_ipc_configuration in

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

let handle_notifications logger stats connection_state compaction_handle notifications = 

  (* 
   * Go over each notifications and compute the client response by looking
   * up the map of pending request. 
   *)
  let connection_state, client_responses = 
    Counter.Perf.f1 (Server_stats.not_processing stats) (fun notifications -> 
      List.fold_left (fun acc notification ->

      match notification with
      | RTypes.Committed_data rev_log_entries -> 
        let ids = List.map (fun ({RPb.id; _ }:RPb.log_entry) -> id) rev_log_entries in 
        List.fold_left (fun (connection_state, client_responses) id -> 
          let {pending_requests; _ }   = connection_state in 
          let pending_requests, client_request = Pending_requests.get_and_remove pending_requests id in 
          let connection_state = {connection_state with pending_requests } in 
          match client_request with
          | None -> 
            (connection_state, client_responses) 
          | Some (_, handle) -> 
            (connection_state, ((APb.Add_log_success, handle)::client_responses))
        ) acc ids 

      | RTypes.New_leader _
      | RTypes.No_leader  -> 
        (* TODO need to go through all the pending requests. 
         *)
        acc
      ) (connection_state, []) notifications  
    ) notifications 
  in

  (* 
   * The RAFT protocol dictates that all commited log entries must be stored
   * permanently on DISK. This way, if a server crashes it can recover. 
   *)
  Lwt_list.iter_s (function
    | RTypes.Committed_data rev_log_entries -> 
      Log_record.append_commited_data logger rev_log_entries compaction_handle 
    | _ -> Lwt.return_unit
  ) notifications
  >|=(fun () -> (connection_state, client_responses))

let send_raft_messages ~logger ~stats id connection_state messages  = 
  let {push_outgoing_message; _ } = connection_state in 

  Lwt_list.map_p (fun ((msg, server_id) as msg_to_send) ->
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
    notifications; 
  }  = result in 

  handle_notifications 
      logger stats connection_state log_record_handle notifications 
  >>=(fun (connection_state, client_responses) -> 
    send_raft_messages 
        ~logger ~stats (raft_state.RTypes.id) 
        connection_state outgoing_messages
    >|= (fun () -> 
      ({state with raft_state; connection_state}, client_responses, [])
    ) 
  )

let handle_raft_message ~logger ~stats ~now state msg = 
  let { raft_state; _ } = state in 

  log ~logger ~level:Notice ~section "Raft Message Received"
  >>=(fun () -> Log.print_state logger section raft_state)
  >>=(fun () -> Log.print_msg_received logger section msg raft_state.RTypes.id)
  >>=(fun () ->
    Server_stats.tick_raft_msg_recv stats;

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
      Printf.printf "NEW LEADER ELECTION [%2i] \n%!" raft_state.RTypes.id;
      log ~logger ~level:Notice ~section "Leader Election timeout"
      >|= (fun () ->
        Raft_logic.handle_new_election_timeout raft_state now
      ))
  end
  >>=(fun result -> 

    process_result logger stats state result  
  ) 

let handle_client_requests ~logger ~stats ~now  state client_requests = 

  let _  = logger and _ = stats and _ = now in 
  let {connection_state; _ } = state in 
  let {pending_requests; _ } = connection_state in 

  let pending_requests, txs = List.fold_left (fun (pending_requests, txs) ((client_request, _ ) as r) ->
    match client_request with
    | APb.Add_tx ({APb.tx_id;_} as tx) -> 
      let pending_requests = Pending_requests.add pending_requests tx_id r in 
      let txs = tx::txs in 
      (pending_requests, txs) 
  ) (pending_requests, []) client_requests in
  
  let app_request = APb.(Validate_txs {txs}) in 

  let connection_state = {connection_state with pending_requests} in 
  Lwt.return (
    {state with connection_state; }, 
    [], 
    [app_request]
  )
      
let process_app_validation logger (pending_requests, validated_client_requests, client_responses) validation = 
  let {
    APb.tx_id; 
    APb.result
  } = validation in 
  
  let pending_requests, client_request = Pending_requests.get_and_remove pending_requests tx_id in 

  match client_request, result with
  | None, _ -> 
    (* This is a violation invariant since no pending request should be removed at
     * this stage. 
     * However not critical for now. 
     *)
    log_f ~logger ~level:Warning ~section "Could not find pending request after validation for tx_id: %s" tx_id 
    >|=(fun () -> 
      (pending_requests, validated_client_requests, client_responses)
    ) 

  | Some ((APb.Add_tx _, _ ) as r), APb.Validation_success -> 
    (* Validation is successful and the corresponding client request  
     * has been retrieved, we can insert start the addition of the request
     * from the RAFT protocol point of view. 
     *) 
    Lwt.return (pending_requests, r::validated_client_requests, client_responses) 

  | Some (_, handle), APb.Validation_failure {APb.error_message; error_code}  -> 
    (* Validation has failed, the log entry is rejected. The failure is propagated 
     * back to the client which initiated the request and the log entry is never
     * created in the RAFT consensus. 
     *)
    log_f ~logger ~level:Notice ~section "Validation failure from App, code: %i, msg: %s" error_code error_message
    >|=(fun () -> 
      let client_response = APb.Add_log_validation_failure in 
      let client_response = (client_response, handle) in 
      (pending_requests, validated_client_requests, client_response::client_responses)
    )

let handle_app_response ~logger ~stats ~now state app_response = 

  let {raft_state; connection_state; _ } = state in 
  let {pending_requests; _} = connection_state in 

  match app_response with
  | APb.Commit_tx_ack _ -> 
    assert(false)
      (* Not implemented yet *)

  | APb.Validations {APb.validations} -> 

    Lwt_list.fold_left_s (process_app_validation logger) (pending_requests, [], []) validations
    >>=(fun (pending_requests, validated_client_requests, client_responses) ->
    
      let datas = List.map (function
        | APb.Add_tx {APb.tx_data; tx_id}, _ -> (tx_data, tx_id) 
      ) validated_client_requests in 

      let new_log_response  = 
        Raft_logic.handle_add_log_entries raft_state datas now 
      in 

      begin match new_log_response with
      | Raft_logic.Delay
      | Raft_logic.Forward_to_leader _ -> 
        log ~logger ~level:Notice ~section "Log Rejected "
        >|= (fun () ->

          let connection_state = {connection_state with pending_requests } in 
          let state = {state with connection_state} in 

          let client_responses = List.fold_left (fun client_responses (_, handle) -> 
            (APb.(Add_log_not_a_leader {
             leader_id = RTypes.current_leader raft_state
            }), handle)::client_responses
          ) client_responses validated_client_requests in  

          (state, client_responses, [])
        )

      | Raft_logic.Appended result -> 
            (* (raft_state, outgoing_messages) -> *) 
        log_f ~logger ~level:Notice ~section "Log Added (log size: %i) (nb logs: %i)" 
          raft_state.RTypes.log.RLog.log_size (List.length datas)  
        >>= (fun () ->

          let pending_requests = List.fold_left (fun pending_requests -> function
            | (APb.Add_tx {APb.tx_id ; _}, _) as r -> 
                Pending_requests.add pending_requests tx_id r 
          ) pending_requests validated_client_requests in 

          let state = {state with 
            connection_state = {connection_state with pending_requests; }
          } in 

          process_result logger stats state result  
        )
      end
    )
