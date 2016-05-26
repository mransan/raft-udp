(** IPC Utilities to receive/send Raft messages via UDP
 *)

type event = 
  | Raft_message of Raft_pb.message
  | Failure 

type next_raft_message_f = unit -> event Lwt.t  

val get_next_raft_message_f_for_server : 
  Raft_udp_pb.configuration -> 
  int ->
  next_raft_message_f 
(** [get_next_raft_message_f_for_server configuration server_id] returns 
    the function [f] to receive the next raft message for [server_id].
 *)

type ipc_handle 

type send_raft_message_f = 
  ipc_handle ->
  Raft_pb.message * int ->
  unit 

val get_send_raft_message_f :
  Raft_udp_pb.configuration ->
  (ipc_handle * send_raft_message_f)
(** [get_send_raft_message_f configuration] returns the function
    [f] to send a Raft message to [server_id].
  *)
