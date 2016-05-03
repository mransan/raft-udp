(** Logging Utilities *) 

(** {2 Logging} *) 

val print_msg_to_send : Lwt_log_core.logger -> int -> Raft_pb.message -> int -> unit Lwt.t  
(** [print_msg_to_send logger from msg to] logs a sent message information
 *)

val print_msg_received : Lwt_log_core.logger -> Raft_pb.message -> int -> unit Lwt.t 
(** [print_msg_received logger msg recipient] logs a receive message information 
 *)
