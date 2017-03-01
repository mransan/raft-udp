(** Module to handle all communication for the RAFT Protocol. *)

(** Current implementation is using UDP and the Protobuf serialization
    from the raft-pb package *)

(** {2 Types} *)

type t 
(** state of the communication system *)

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
(** [send ~states ipc messages] enqueues [messages] to be sent 
    asynchronously *)

type event = 
  | Raft_message of Raft_pb.message
  | Failure 
(** Return for new messages. [Failure] is return if messages could not be 
    decoded *)

val get_next : t -> event Lwt.t
(** Return promise to the next RAFT event *)
