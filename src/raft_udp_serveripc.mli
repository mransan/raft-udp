(** Server IPC 
 
    This module handles all the RAFT server events (raft msgs, timeout and 
    client requests.
  *)

(** {2 Types} *)

type raft_message     = Raft_pb.message * int 

type raft_messages    = raft_message list 

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

type state = Raft_pb.state * connection_state * Raft_udp_compaction.handle  

(** {2 Event handling} *)

val initialize : unit -> connection_state 
(** [initialize ()] returns an empty connection state *)

val handle_raft_message :
  logger: Lwt_log_core.logger    ->
  stats : Raft_udp_serverstats.t ->
  now   : float -> 
  state -> 
  Raft_pb.message ->
  (state * raft_messages * client_responses) Lwt.t  
(** [handle_raft_message ~logger ~stats ~now state msg] handles RAFT protocol 
    messages from other servers. 
  *)

val handle_timeout :
  logger: Lwt_log_core.logger    ->
  stats : Raft_udp_serverstats.t ->
  now   : float -> 
  state ->
  Raft_pb.timeout_event_time_out_type ->
  (state * raft_messages * client_responses) Lwt.t  
(** [handle_raft_message ~logger ~stats ~now state msg] handles RAFT protocol 
    timeout event. 
  *)

val handle_client_request :
  logger: Lwt_log_core.logger    ->
  stats : Raft_udp_serverstats.t ->
  now   : float -> 
  state ->
  client_request ->
  (state * raft_messages * client_responses) Lwt.t  
(** [handle_client_request ~logger ~stats ~now state msg] handles RAFT protocol 
    client requests. 
  *)
