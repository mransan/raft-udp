(** Generic client interface to add transaction to the Raft 
    Consensus. 
  *)

(** Application signature for Application specific transaction type.  *)
module type App_sig = sig 
  type data 
  val encode : data -> bytes 

  type result 
  val decode : bytes -> result 
end 

type t 

val make : Raft_com_conf.t -> t Lwt.t 

module Make(App:App_sig) : sig 

  val send : t -> App.data -> App.result option Lwt.t 

end
