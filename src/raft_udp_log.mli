(** Logging Utilities *) 

(** {2 Logging} *) 

val print_msg_to_send : Lwt_log_core.logger -> Lwt_log_core.section -> int -> Raft_pb.message -> int -> unit Lwt.t  
(** [print_msg_to_send logger from msg to] logs a sent message information
  *)

val print_msg_received : Lwt_log_core.logger -> Lwt_log_core.section -> Raft_pb.message -> int -> unit Lwt.t 
(** [print_msg_received logger msg recipient] logs a receive message information 
  *)

val print_state : Lwt_log_core.logger -> Lwt_log_core.section -> Raft_state.t -> unit Lwt.t
(** [print_state logger state] logs the server state. 
  *)
