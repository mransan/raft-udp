(** Utility functions for managing TCP connections *)

val read_msg_with_header : Lwt_io.input_channel -> bytes -> (bytes * int) Lwt.t
(** [read_msg_with_header ic buffer] reads a new message from [ic] where 
    the msg has a length prefix encoded in 4 bytes/big endian. This 
    function can raises the exception as [Lwt_io.read]. 

    If the message length fits into [buffer] then buffer is used to 
    read the message from [ic], otherwise a new buffer of sufficient size 
    is created. 
    
    The function returns [(buffer, len)], with [len] being the length of the 
    message. One can use [Bytes.sub buffer len] to extract the message. *)

val write_msg_with_header : Lwt_unix.file_descr -> bytes -> unit Lwt.t 
(** [write_msg_with_header fd msg] writes [msg] to [fd] prefixing 
    [msg] with its length encoded in 4 bytes/big endian. 

    @raises exception if an error occured. *)
