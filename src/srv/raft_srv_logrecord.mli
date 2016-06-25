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

val make : Lwt_log_core.logger -> Raft_udp_pb.configuration -> int -> t Lwt.t 
(** [make configuration] initialize the disk based log recoding. 
    
    returns a handle that client application should keep track 
    of in order to subsequently call the [append_commited_data] function
  *)

val append_commited_data : Lwt_log_core.logger -> Raft_pb.log_entry list -> t -> unit Lwt.t 
(** [append_commited_data log_entries handle] permanently record the [log_entries]. 
  *)


val read_log_records : 
  Raft_udp_pb.configuration -> 
  int -> 
  ('a -> Raft_pb.log_entry -> 'a) -> 
  'a -> 
  'a Lwt.t
(** [read_log_records configuration server_id f e0] folds over all the records 
    stored permanently. 

    If no previous record are found on the permanent storage then 
    [e0] is returned.
  *)
