(** Log Recording on Disk functionality 
 
    After a [log_entry] was found to be commited by the RAFT protocol 
    and before the commit response is sent back to the client, the log 
    entry must be saved permanently. 
    
    This permanent record of [log_enrtry]s is then used when a server 
    restarts and recover its previous state. 

    This module implements a disk based permanent recording of 
    [log_entry]s. 
  *)

type t 
(** Handle to be maintain by client application. 
  *)

val make : 
  logger:Lwt_log_core.logger -> 
  Raft_com_pb.configuration -> 
  int -> 
  t Lwt.t 
(** [make logger configuration server_id] initialize the disk based log recoding. 
    
    returns a handle that client application should keep track 
    of in order to subsequently call the [append_commited_data] function
  *)

val close : t -> unit Lwt.t 
(** [close handle] closes the handle and resources associated with it. [handle]
    cannot be re-used after. 
  *)

val append_commited_data : 
  logger:Lwt_log_core.logger -> 
  rev_log_entries:Raft_pb.log_entry list -> 
  t -> 
  unit Lwt.t 
(** [append_commited_data ~logger ~rev_log_entries handle] permanently record the [log_entries]. 
    
    As the [rev_log_entries] name suggest, the entries are expected to be in
    chronological order. 
  *)


val read_log_records : 
  logger:Lwt_log_core.logger -> 
  Raft_com_pb.configuration -> 
  int -> 
  ('a -> Raft_pb.log_entry -> 'a) -> 
  'a -> 
  'a Lwt.t
(** [read_log_records configuration server_id f e0] folds over all the records 
    stored permanently. 

    If no previous record are found on the permanent storage then 
    [e0] is returned.
  *)
