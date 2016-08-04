(** Global Compaction logic *)

val perform_compaction : 
  Lwt_log_core.logger -> 
  Raft_com_pb.configuration ->
  Raft_state.t -> 
  Raft_pb.log_interval list Lwt.t 
(** [perform_compaction logger state] compact or expands the required 
    log intervals. (As returned by Raft_state.compaction_report). 

    The function returns the list of [log_intervals] which were modified. 
  *)

val update_state : 
  Lwt_log_core.logger ->
  Raft_pb.log_interval list -> 
  Raft_state.t -> 
  Raft_state.t Lwt.t 
(** [update_state logger modified_log_intervals state] replace the [modified_log_intervals]
    in [state] and returns the new state. 
  *)

val load_previous_log_intervals : 
  Lwt_log_core.logger ->
  Raft_com_pb.configuration ->
  int ->
  Raft_pb.log_interval list Lwt.t 
(** [load_previous_log_intervals] loads from disk all the saved [log_interval]. This
    function should be called upon restarting a server. 

    Note that all the [log_interval] are returned in [Compacted] form.
  *)
