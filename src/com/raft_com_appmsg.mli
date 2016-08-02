(** Common utility function for the messages between RAFT and APP servers *)



val write_message_header : 
  message_size:int -> 
  Lwt_unix.file_descr -> 
  unit 
  -> unit Lwt.t 
(** [write_message_header ~message_size fd ()] sends the header part of the message
  *)

type header 

val message_size : header -> int 
(** [message_size header] returns the message size information from [header] *)

val read_message_header : Lwt_unix.file_descr -> header Lwt.t  
(** [read_message_header fd] reads the header part of the message *)
