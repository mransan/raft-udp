(** Client IPC logic 
 
    This module handles all the TCP based communication with
    the clients. 
 *)

type handle 
(** A handle to the client connection so that response can be dispatched
  * to the right client. 
  *
  * Internally this is the file descriptor. 
  *)

type client_request  = Raft_udp_pb.client_request * handle 
(** Client request type *)

type send_response_f = (Raft_udp_pb.client_response * handle) option -> unit 
(** Sender function for the caller to send a response 
 *)

val client_request_stream : 
  Lwt_log_core.logger -> 
  Raft_udp_pb.configuration ->
  Raft_udp_serverstats.t ->
  int -> 
  client_request Lwt_stream.t * send_response_f 
(** [client_request_stream ~logger configuration server_id] initialize the 
    TCP based client IPC to process incoming client request to the RAFT
    server. 

    The function return a stream or request which will be closed upon
    a fatal error and a send response function.  
  *)
