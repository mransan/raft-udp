(** Server IPC 
 
    This module handles all the RAFT server events (raft msgs, timeout and 
    client requests.
  *)

(** {2 Types} *)


type client_request   = Raft_com_pb.client_request * Raft_srv_clientipc.handle

type client_response  = Raft_com_pb.client_response * Raft_srv_clientipc.handle 

type client_responses = client_response list 

type app_requests = Raft_com_pb.app_request list 

type app_response = Raft_com_pb.app_response 

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
  raft_state: Raft_types.state; 
  connection_state: connection_state; 
  log_record_handle: Raft_srv_logrecord.t
}

type result = (state * client_responses * app_requests) 
(* TODO check if there is ever a need for more than one app_request 
 * ie maybe chane to app_request option rather than list
 * TODO: check again based on the recent change that we only 
 * send a single request after we get a response. *)

(** {2 Event handling} *)

val initialize : Raft_com_conf.t -> int -> (connection_state * app_requests) 
(** [initialize ()] returns an empty connection state *)

val get_next_raft_message : 
  state -> 
  Raft_srv_raftipc.event Lwt.t 

val handle_raft_message :
  stats : Raft_srv_serverstats.t ->
  now   : float -> 
  state -> 
  Raft_pb.message ->
  result Lwt.t  
(** [handle_raft_message ~stats ~now state msg] handles RAFT protocol 
    messages from other servers. 
  *)

val handle_timeout :
  stats : Raft_srv_serverstats.t ->
  now   : float -> 
  state ->
  Raft_types.timeout_type ->
  result Lwt.t  
(** [handle_raft_message ~stats ~now state msg] handles RAFT protocol 
    timeout event. 
  *)

val handle_client_requests :
  stats : Raft_srv_serverstats.t ->
  now   : float -> 
  state ->
  client_request list ->
  result Lwt.t  
(** [handle_client_request ~stats ~now state msg] handles RAFT protocol 
    client requests. 
  *)

val handle_app_response :
  stats : Raft_srv_serverstats.t ->
  now   : float -> 
  state ->
  app_response ->
  result Lwt.t  
(** [handle_app_response ~stats ~now state msg] handles the response received
    from the application server. 
  *)
