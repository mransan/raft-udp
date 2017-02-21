(** Generic client interface to add transaction to the Raft 
    Consensus. 
  *)

(** Application signature for Application specific transaction type.  *)
module type App_sig = sig 
  type log 
  val encode : log -> bytes 
end 

type t 

val make : Lwt_log_core.logger -> Raft_com_conf.t -> t Lwt.t 

type send_result =  (unit, string) Result.result

module Make(App:App_sig) : sig 

  val send : t -> App.log -> send_result Lwt.t 

end
