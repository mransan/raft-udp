(** Generic App server implementation *)

(** This module implements the protocol between the App sever and the Raft server 
    generically. It is parametrized by a module which simply specify 
    how to decode the Application specific transaction data. 
  *)

(** Module signature to be implemented by each specific application. 
 *)
module type App_sig  = sig 

  type log_data 

  val decode : bytes -> log_data  

end

type validation_result = 
  | Ok 
  | Error of string 

type log_validation = {
  id : string; 
  result : validation_result; 
} 

module Make(App:App_sig) : sig 

  type log = {
    id : string; 
    data : App.log_data;
  } 
  (** transaction type *)

  type validations = log list * (log_validation list -> unit) 
  (** validation request to be processed by the application. The first value
    * is the list of the transaction to be validated while the second argument
    * is the function callback for the application to notify the result of the 
    * validation. 
    *)

  val start : 
    Lwt_log_core.logger -> 
    Raft_com_conf.t -> int -> validations Lwt_stream.t 
  (** [start logger configuration] returns the continuous stream of request to be validated
    * by the specific application. 
    *)

end (* Make *) 
