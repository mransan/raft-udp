(** Generic App server implementation *)

(** This module implements the protocol between the App sever and the Raft server 
    generically. It is parametrized by a module which simply specify 
    how to decode the Application specific transaction data. 
  *)

(** Module signature to be implemented by each specific application. *)
module type App_sig  = sig 

  type data 

  val decode : bytes -> data

  type result 

  val encode : result -> bytes 

end


module Make(App:App_sig) : sig 

  type log = {
    id : string; 
    index : int; 
    app_data : App.data;
  } 

  type log_result = {
    id : string; 
    index : int; 
    app_result : App.result option; 
  } 

  type add_log_entries = log list * (log_result list -> unit) 
  (** validation request to be processed by the application. The first value
    * is the list of the transaction to be validated while the second argument
    * is the function callback for the application to notify the result of the 
    * validation. *)

  val start : 
    Raft_com_conf.t -> int -> add_log_entries Lwt_stream.t 
  (** [start configuration] returns the continuous stream of request to 
    * be validated by the specific application. *)

end (* Make *) 
