(** Server IPC 
 
    This module handles all the RAFT server events (raft msgs, timeout and 
    client requests.
  *)

(** {2 Types} *)

type event = 
  | Raft_message of Raft_pb.message
  | Failure 

type next_raft_message_f = unit -> event Lwt.t  

type client_request   = Raft_udp_pb.client_request * Raft_udp_clientipc.handle

type client_response  = Raft_udp_pb.client_response * Raft_udp_clientipc.handle 

type client_responses = client_response list 

type connection_state 
(** Abstract type which internally capture the necessary information to 
    be maintain between all the RAFT events. 

    In particular a [connection_state] keeps track of the outstanding 
    [Add_log] client request. The response to this request can only
    be sent when the data is commited (ie replicated on a majority 
    of nodes). Therefore the response will not be returned immediately
    and some kind of state needs to be kept. 
  *)

type state = {
  raft_state: Raft_pb.state; 
  connection_state: connection_state; 
  log_record_handle: Raft_udp_logrecord.t
}

(** {2 Event handling} *)

val get_next_raft_message_f_for_server : 
  Raft_udp_pb.configuration -> 
  int ->
  next_raft_message_f 
(** [get_next_raft_message_f_for_server configuration server_id] returns 
    the function [f] to receive the next raft message for [server_id].
 *)

val initialize : Raft_udp_pb.configuration -> connection_state 
(** [initialize ()] returns an empty connection state *)

val handle_raft_message :
  logger: Lwt_log_core.logger    ->
  stats : Raft_udp_serverstats.t ->
  now   : float -> 
  state -> 
  Raft_pb.message ->
  (state * client_responses) Lwt.t  
(** [handle_raft_message ~logger ~stats ~now state msg] handles RAFT protocol 
    messages from other servers. 
  *)

val handle_timeout :
  logger: Lwt_log_core.logger    ->
  stats : Raft_udp_serverstats.t ->
  now   : float -> 
  state ->
  Raft_pb.timeout_event_time_out_type ->
  (state * client_responses) Lwt.t  
(** [handle_raft_message ~logger ~stats ~now state msg] handles RAFT protocol 
    timeout event. 
  *)

val handle_client_request :
  logger: Lwt_log_core.logger    ->
  stats : Raft_udp_serverstats.t ->
  now   : float -> 
  state ->
  client_request ->
  (state * client_responses) Lwt.t  
(** [handle_client_request ~logger ~stats ~now state msg] handles RAFT protocol 
    client requests. 
  *)