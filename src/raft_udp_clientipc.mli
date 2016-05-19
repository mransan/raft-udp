
val client_request_stream : 
  Lwt_log_core.logger -> 
  Raft_udp_pb.configuration ->
  int -> 
  Raft_udp_pb.client_request Lwt_stream.t 
