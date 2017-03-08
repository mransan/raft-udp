(** Persistent Storage for the RAFT protocol *)
 
(** {2 Types} *)

type t 
(** Handle to be maintain by client application.  *)

(** {2 Creators} *)

val make : Raft_com_conf.t -> int -> t Lwt.t 
(** [make configuration] initialize the disk based storage. *)

(** {2 Log Storage} *)

val add_logs : 
  Raft_log.log_entry list -> 
  t -> 
  unit Lwt.t 
(** [add_logs log_entries storage] permanently records the [log_entries].*)

val set_committed :
  Raft_log.log_entry list ->
  t ->
  unit Lwt.t 
(** [set_committed log_entries storage] udpates the permanent records of 
    the logs to be committed. *)

val delete_logs : 
  Raft_log.log_entry list -> 
  t -> 
  unit Lwt.t
(** [delete_logs log_entries storage] deletes the given [log_entry]
    in the permanent storage *)

(** {2 State Storage} *)

val read_raft_state : 
  now:float -> 
  t -> 
  Raft_types.state Lwt.t 
(** [read_raft_state ~now storage] initializes a the raft state based on
    the data persisted *)
