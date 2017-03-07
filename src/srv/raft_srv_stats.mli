(** Statistics  collection and display *)


(** {2 Counter/Rate} *)

val raft_msg_send : Raft_utl_counter.Counter.t 

val raft_msg_recv : Raft_utl_counter.Counter.t 

val log : Raft_utl_counter.Counter.t 

val heartbeat : Raft_utl_counter.Counter.t

val client_connections : Raft_utl_counter.Counter.t 

val client_requests : Raft_utl_counter.Counter.t

val append_entries_failure : Raft_utl_counter.Counter.t

(** {2 Performance Measurement} *)

val msg_processing : Raft_utl_counter.Perf.t 

val hb_processing : Raft_utl_counter.Perf.t 

val add_log_processing : Raft_utl_counter.Perf.t

(** {2 Server State} *)

type server_role =
  | Leader
  | Follower
  | Candidate

val server_role : server_role option ref 

val server_id : int ref 

(** {2 Configuration} *)

val print_header : bool ref
