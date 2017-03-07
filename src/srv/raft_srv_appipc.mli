(** IPC for the RAFT <-> App Server Protocol *) 
 
(** The IPC is based on TCP and using Raft_com_pb protobuf types.*)

type send_app_request_f  = Raft_com_pb.app_request option -> unit 
(** Function to send a request to the APP server. The corresponding response 
    will be returned in the response stream.  *)

type t = send_app_request_f * Raft_com_pb.app_response Lwt_stream.t 
(** Type for all the App server communication (ie send request/receive 
    response *) 

val make : 
  Raft_com_conf.t -> 
  int -> 
  Raft_srv_serverstats.t -> 
  t 
(** [make configuration stats] initialize and return the IPC with the 
    APP server. *) 
