(** Generic client interface to add transaction to the Raft 
    Consensus. 
  *)

(** Application signature for Application specific transaction type. 
  *)
module type App_sig = sig 

  type tx 

  val encode : tx -> bytes 

end 

type t 

val make : Lwt_log_core.logger -> Raft_com_pb.configuration -> t Lwt.t 

type send_result = 
  | Send_result_ok 
  | Send_result_error of string 

module Make(App:App_sig) : sig 

  val send : t -> App.tx -> send_result Lwt.t 

end
