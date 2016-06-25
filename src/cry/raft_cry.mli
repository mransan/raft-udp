module Sig : sig 

  type t 
  (* Signature type *)

  val serialize   : t -> string 
  (** [serialize s] Serializes the signature [s] to a plain text representation *)

  val deserialize : string -> t 
  (** [deserialize s] Deserializes the string [s] into a signature 
    *    
    * Note that [s] must have previously been computed with [serialize] function, if 
    * not behavior is undefined. 
    *) 

end 

module Pub : sig 

  type  t 
  (** Public key type *)

  val verify : t -> string -> Sig.t -> bool 
  (** [verify public_key msg signature] returns [true] if [signature] is a valid
    * signature of the [msg], [false] otherwise.
    *)

  val serialize : t -> string 

  val deserialize : string ->  t

end 

module Prv : sig 

  type t 
  (** Private key type *)

  val make : unit -> t
  (** [make ()] create a new random key *)

  val public_key : t -> Pub.t 
  (** [public_key private_key] returns the public key associated with [private_key]
    *)

  val sign : t -> string -> Sig.t 
  (** [sign private_key msg] compute the signature for [msg] *)

  val serialize : t -> string 

  val deserialize : string ->  t

end 

module Sha256 : sig 

  val hash_strings : string list -> string 

end 
