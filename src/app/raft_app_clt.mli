(** Generic client interface to add logs to a RAFT cluster *)

(** {2 Types} *)

(** Application module type which defines both the log data and 
    log response along with their encoding *)
module type App_sig = sig 
  type data 
  val encode : data -> bytes 

  type result 
  val decode : bytes -> result 
end 

(** IPC type *)
type t 

(** {2 Creator} *)

val make : Raft_com_conf.t -> t Lwt.t 
(** [make configuration] initializes the client IPC with the RAFT cluster 
    defined in [configuration]. *)

module Make(App:App_sig) : sig 

  val send : t -> App.data -> App.result option Lwt.t 
  (** [send client_ipc log_data] returns a promise to the result of adding
      [log_data] to the RAFT cluster. 
      
      If the process exit before this function returns [log_data] might
      have been added. 
      
      There are no guarantee on the time that the function will take to 
      add the log. In fact this function liveness guarantee are the ones
      of the RAFT protocol. *)
end
