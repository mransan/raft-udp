(** Encoding/Decoding functionality *)

(** 32 bit little endian int encoding *)

exception Overflow 
(** Exception raised when (Bytes.length b) < pos + encoding_size *)

module Int32LE : sig 

  type t  = int32 

  val size : int 
  (** size in bytes of the encoded value *)

  val decode : int -> bytes -> t 
  (** [decode pos bytes] decodes a value of type [t] from [bytes] starting at [pos] *) 

  val encode : int -> bytes -> t -> unit 
  (** [encode v pos bytes] encodes [v] in [bytes] starting at [pos] *) 

end 
