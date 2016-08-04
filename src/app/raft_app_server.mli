(** Generic App server implementation *)

(** This module implements the protocol between the App sever and the Raft server 
    generically. It is parametrized by a module which simply specify 
    how to decode the Application specific transaction data. 
  *)

(** Module signature to be implemented by each specific application. 
 *)
module type App_sig  = sig 

  type tx_data 

  val decode : bytes -> tx_data  

end

type validation_result = 
  | Validation_result_ok 
  | Validation_result_error of string 

type tx_validation_result = {
  tx_id : string; 
  result : validation_result; 
} 

module Make(App:App_sig) : sig 

  type tx = {
    tx_id : string; 
    tx_data : App.tx_data;
  } 
  (** transaction type *)

  type validations = tx list * (tx_validation_result list -> unit) 
  (** validation request to be processed by the application. The first value
    * is the list of the transaction to be validated while the second argument
    * is the function callback for the application to notify the result of the 
    * validation. 
    *)

  val start : 
    Lwt_log_core.logger -> 
    Raft_udp_pb.configuration -> 
    int -> 
    validations Lwt_stream.t option 
  (** [start logger configuration server_id] returns the continuous stream of request to be validated
    * by the specific application.
    *
    * [None] is returned on failure while [Some validation_stream] is returned
    * on success. 
    *)

end (* Make *) 
