(** Server performance statistics  *)

(** {2 Types} *)

type t
(** Statistic collector *)

(** {2 Creators} *)

val make : ?print_header:unit -> initial_log_size:int -> id:int -> unit -> t 
(** [make ~initial_log_size ()] create the statistic collector
  *)

(** {2 Stats collections} *)

val tick_raft_msg_send : t -> unit 

val tick_raft_msg_recv : t -> unit 

val set_log_count : t -> int -> unit 

val tick_heartbeat : t -> unit 

val tick_new_client_connection : t -> unit 

val tick_append_entries_failure : t -> unit 

val msg_processing : t -> Raft_udp_counter.Perf.t 

val hb_processing : t -> Raft_udp_counter.Perf.t 
