(** Lwt related functionality *)

(** {2 Logger} *) 

val make_logger : ?to_file:string -> unit -> Lwt_log_core.logger Lwt.t 

val tap : ('a -> unit Lwt.t) -> 'a -> 'a Lwt.t 
