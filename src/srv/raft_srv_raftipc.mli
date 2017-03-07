(** IPC for the RAFT protocol *)

(** The IPC is based on UDP and using Raft_pb protobuf types *)

(** {2 Types} *)

type t 
(** IPC type *)

(** {2 Creators} *)

val make : Raft_com_conf.t -> int -> t 
(** [make configuration server_id] creates a new instance of the 
    communication system *)

(** {2 Communication API} *)

val send : 
  stats:Raft_srv_serverstats.t -> 
  t -> 
  (Raft_types.message * int) list -> 
  unit 
(** [send ~stats ipc messages] enqueues [messages] to be sent 
    asynchronously *)

type event = 
  | Raft_message of Raft_pb.message
  | Failure 
(** Return for new messages. [Failure] is return if messages could not be 
    decoded *)

val get_next : t -> event Lwt.t
(** Return promise to the next RAFT event *)
