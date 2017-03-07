(** IPC for the RAFT <-> App Server Protocol *) 
 
(** The IPC is based on TCP and using Raft_com_pb protobuf types.*)


type app_request = Raft_com_pb.app_request 
(** App request *)

type app_response = Raft_com_pb.app_response
(** App response *)

type t 
(** IPC type *) 

val make : 
  Raft_com_conf.t -> 
  Raft_srv_stats.t -> 
  int -> 
  t 
(** [make configuration stats server_id] initialize the IPC state with 
    the App server. *) 

val get_next : t -> app_response option Lwt.t 
(** [get_next app_ipc] returns promise to the next response from the 
    App server. *)

val send : t -> app_request list -> unit   
(** [send app_ipc app_request] enqueues [app_request] to be send 
    asynchronously. *)
