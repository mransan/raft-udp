(** Configuration utilities. *)

val default_configuration : 
  unit -> 
  Raft_udp_pb.configuration

val sockaddr_of_server_id : 
  Raft_udp_pb.configuration ->
  int -> 
  Unix.sockaddr option

val sockaddr_of_server_config: 
  Raft_udp_pb.server_udp_configuration->
  Unix.sockaddr
