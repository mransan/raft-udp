(** IPC Utilities to receive/send Raft messages via UDP
 *)

type 'a next_raft_message_f = 
  unit -> 
  ([> `Raft_message of Raft_pb.message | `Failure] as 'a) Lwt.t  

val get_next_raft_message_f_for_server : 
  Raft_udp_pb.configuration -> 
  int ->
  'a next_raft_message_f 
(** [get_next_raft_message_f_for_server configuration server_id] returns 
    the function [f] to receive the next raft message for [server_id].
 *)

type send_raft_message_f = 
  Raft_pb.message ->
  int ->
  unit Lwt.t 

val get_send_raft_message_f :
  Raft_udp_pb.configuration ->
  send_raft_message_f 
(** [get_send_raft_message_f configuration] returns the function
    [f] to send a Raft message to [server_id].
  *)
