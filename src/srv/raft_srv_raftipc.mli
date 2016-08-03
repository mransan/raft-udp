type t 
(** IPC handle for RAFT *)

val make : 
  logger:Lwt_log_core.logger -> 
  stats:Raft_srv_serverstats.t -> 
  configuration:Raft_udp_pb.configuration -> 
  server_id:int ->
  unit ->
  t 
(** [make ~logger ~configuration ~stats ()] creates and initializes the 
  * RAFT IPC. 
  *)

val next_message : t -> (Raft_pb.message, string) result Lwt.t  
(** [next_message t] returns the next RAFT message sent to 
  * this server.
  *)

val send_message : t -> Raft_pb.message -> int -> unit  
(** [send_raft_message t message server_id] schedule [message] to be
  * sent asynchronously to [server_id]. 
  *)

val send_messages : t -> (Raft_pb.message * int) list -> unit 
(** [send_raft_messages t messages] schedule the [messages] to be 
  * sent asynchronously. 
  *)
