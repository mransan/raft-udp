(** Utilities around Lwt default logger *)


val start : basename:string -> interval:int -> unit -> unit Lwt.t 
(** [start ~basename ~interval ()] returns a perpetual thread which 
    implements a rotating Lwt logger every [interval] seconds. Only the 3 
    latest log file are kept. The log file name format is 
    <basename>_<timestamp>.log *)
