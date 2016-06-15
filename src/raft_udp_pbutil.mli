(** Small utilities around Raft_udp_pb protobuf types *)

val string_of_debug_info : Raft_udp_pb.app_ipc_debug -> string 
(** [string_of_debug_info debug_info] returns a pretty print string *)

val string_of_app_request : Raft_udp_pb.app_request -> string 
(** [string_of_app_request app_request] returns a pretty print string *)

val string_of_app_response : Raft_udp_pb.app_response -> string 
(** [string_of_app_response app_response] returns a pretty print string *)
