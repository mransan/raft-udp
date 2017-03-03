(** Rate limiter for reading from an Lwt_stream.t *) 

val wrap : int -> 'a Lwt_stream.t -> (unit -> 'a list Lwt.t)
(** [wrap rate stream] returns a pop function which can be used 
    by the caller to get the next available elements from 
    the stream. If the pop function returns [[]] then the stream 
    is closed *)
