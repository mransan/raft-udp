(** Server IPC 
 
    This module handles all the RAFT server events (raft msgs, timeout and 
    client requests.
  *)

(** {2 Types} *)

type raft_message    = Raft_pb.message * int 

type raft_messages   = raft_message list 

type client_request  = Raft_udp_pb.client_request * Raft_udp_clientipc.handle

type client_response = Raft_udp_pb.client_response * Raft_udp_clientipc.handle 

type client_responses = client_response list 

(** {2 Event handling} *)

val handle_raft_message :
  logger: Lwt_log_core.logger    ->
  stats : Raft_udp_serverstats.t ->
  now   : float -> 
  Raft_pb.state ->
  Raft_pb.message ->
  (Raft_pb.state * raft_messages * client_responses) Lwt.t  

val handle_timeout :
  logger: Lwt_log_core.logger    ->
  stats : Raft_udp_serverstats.t ->
  now   : float -> 
  Raft_pb.state ->
  Raft_pb.timeout_event_time_out_type ->
  (Raft_pb.state * raft_messages * client_responses) Lwt.t  

val handle_client_request :
  logger: Lwt_log_core.logger    ->
  stats : Raft_udp_serverstats.t ->
  now   : float -> 
  Raft_pb.state ->
  client_request ->
  (Raft_pb.state * raft_messages * client_responses) Lwt.t  
