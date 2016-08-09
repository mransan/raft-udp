(** Generic client interface to add transaction to the Raft 
    Consensus. *)

(** Application signature for Application specific transaction type. *)
module type App_sig = sig 

  type tx 

  val encode : tx -> bytes 

end 

type t 
(** Connection to the RAFT servers *)

val make : Lwt_log_core.logger -> Raft_com_pb.configuration -> t Lwt.t 

type send_result = 
  | Send_result_app_ok 
    (** The tx was recorded and the application logic returned ok *)

  | Send_result_app_error of string 
    (** The tx was recorded and the application logic returned an error *)

  | Send_result_internal_error of string 
    (** The tx might have been recorded. An internal error made it impossible
        to verify wether it has been recorded and what was the result returned
        by the application logic. However subsequent tx are not affected by 
        this failure.  *)

  | Send_result_failure 
    (** The connection is not functioning, the tx was not recorded *)

module Make(App:App_sig) : sig 

  val send : t -> App.tx -> send_result Lwt.t 

end
