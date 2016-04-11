(** Counter utility for measuring rate of an recurrent event
 *)


(** {2 Types} *) 

type t 
(** Counter type. This is a mutable data structure *)

(** {2 Creators} *) 

val make: ?initial_counter:int -> unit -> t 
(** [make ~initial_counter ()] creates a new counter. *)


(** {2 Accessors} *) 

val incr : t -> unit 
(** [incr counter] increments the counter with a single tick*)

val set : t -> int -> unit 
(** [set counter v] set the counter value to [v]. *)

val rate : t -> float 
(** [rate counter] returns the average rate per seconds since 
    the last invocation of [rate].
  *)

val value : t -> int 
(** [value counter] returns the [counter] value. *)
