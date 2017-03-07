(** Debugging Utilities *)

(** {2 String-of} *)

val string_of_app_request : Raft_com_pb.app_request -> string 
(** [string_of_app_request app_request] returns a pretty print string *)

val string_of_app_response : Raft_com_pb.app_response -> string 
(** [string_of_app_response app_response] returns a pretty print string *)

(** {2 Logging} *) 

val print_msg_to_send : 
  Lwt_log_core.section -> 
  Raft_pb.message -> 
  int -> 
  unit Lwt.t  
(** [print_msg_to_send msg to] logs a sent message information *)

val print_msg_received : 
  Lwt_log_core.section -> 
  Raft_pb.message -> 
  unit Lwt.t 
(** [print_msg_received msg ] logs a receive message information *)

val print_state : 
  Lwt_log_core.section -> 
  Raft_types.state -> 
  unit Lwt.t
(** [print_state state] logs the server state.  *)
