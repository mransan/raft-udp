(** Simple implementation of an Asset application *)

type t 
(** Application type which handles all the transaction and store 
    the state of all assets
  *)

val make : unit -> t 
(** [make ()] creates a new application *)

val pp : Format.formatter -> t -> unit  
(** [pp fmt app] formats [app] *)

val show : t -> string 
(** [show app] pretty prints the application to a string *)

val content_of_url : string -> string Lwt.t 
(** [content_of_url url] fetch and return the content of the data 
    pointed to by [url].
  *)

val handle_tx : 
  logger:Lwt_log_core.logger -> 
  t -> 
  Asset_pb.tx -> 
  (t, string) result Lwt.t
(** [handle_tx app tx] handles a transaction and update the application
  *)
