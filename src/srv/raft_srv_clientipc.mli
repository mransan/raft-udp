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

type client_request = Raft_app_pb.client_request * handle
(* type client_request  = Raft_udp_pb.client_request * handle 
 *)

(** Client request type *)

type send_response_f = (Raft_app_pb.client_response * handle) option -> unit 
(** Sender function for the caller to send a response 
 *)


type t = client_request Lwt_stream.t * send_response_f 
(** Client IPC type. The Client IPC consists in 2 parts:
    
    {ul
    {- A stream of request which the application should iterate over}
    {- A function to send back responses.}  
    }
  *)

val make : 
  Lwt_log_core.logger -> 
  Raft_udp_pb.configuration ->
  Raft_srv_serverstats.t ->
  int -> 
  t
(** [client_request_stream ~logger configuration server_id] initialize the 
    TCP based client IPC to process incoming client request to the RAFT
    server. 

    The function return a stream or request which will be closed upon
    a fatal error and a send response function.  
  *)
