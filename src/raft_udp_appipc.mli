(** Application IPC handles the communication between the RAFT server and the App server 
 
    In RAFT application the APP server is responsible to handle the validation of the 
    log data and to apply the log data to its state. 
  *)

(*
type t 

val add_validation : Raft_udp_pb.log_entry -> t -> unit   

val get_
*)

type send_app_request_f  = Raft_udp_pb.app_request option -> unit 
(** Function to send a request to the APP server. The corresponding response 
    will be returned in the response stream. 
  *)

type t = send_app_request_f * Raft_udp_pb.app_response Lwt_stream.t 
(** Type for all the App server communication (ie send request/receive response). *)

val make : 
  Lwt_log_core.logger ->
  Raft_udp_pb.configuration -> 
  Raft_udp_serverstats.t -> 
  t 
(** [make logger configuration stats] initialize and return the IPC with the APP server. *) 