(** Generic App server implementation *)

(** This module implements the protocol between the App sever and the 
    Raft server generically. It is parametrized by a module which 
    simply specify how to decode the Application specific 
    transaction data. *)

(** Module signature to be implemented by each specific application. *)
module type App_sig  = sig 

  type data 
  (** Data associated with each new log entry *)

  val decode : bytes -> data
  (** Decoding function *)

  type result 
  (** Result data of the log application in the application *)

  val encode : result -> bytes 
  (** Encoding function *)

end

module Make(App:App_sig) : sig 

  type log_entry = {
    id : string; 
    index : int; 
    app_data : App.data;
  } 

  type log_result = {
    id : string; 
    index : int; 
    app_result : App.result option; 
  } 

  type add_log_entries = log_entry list * (log_result list -> unit) 
  (** add-log requests to be processed by the application. The first value
      is the list of logs to be processed chronologically by the application, 
      while the second value is the function callback for the application to 
      notify the result of the validation. *)

  val start : Raft_com_conf.t -> int -> add_log_entries Lwt_stream.t 
  (** [start configuration] returns the continuous stream of request to 
      be validated by the specific application. *)

end (* Make *) 
