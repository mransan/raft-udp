(** Counter utility for measuring rate of an recurrent event
 *)


(** {2 Types} *) 

module Counter : sig

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

end (* Counter *)


module Perf : sig

  type t 

  val make : unit ->  t

  val add : t -> float -> unit 

  val f1 : t -> ('a -> 'b) -> 'a -> 'b  

  val f2 : t -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c 
  
  val f3 : t -> ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd 

  val stats : ?reset:unit -> t -> (float * float * float * int)  
  (** [stats ~reset:() t] returns [(min, max, avg, count) for the current
   * period. if [reset] argument is given then the period is reset. 
   *) 
  
  val avg : ?reset:unit -> ?unit_:[`Us | `Ms]  -> t -> float 
 
end 
