(** demo.proto Generated Types and Encoding *)

(** {2 Types} *)

type tx = {
  counter_value : int;
  process_id : int;
}


(** {2 Default values} *)

val default_tx : 
  ?counter_value:int ->
  ?process_id:int ->
  unit ->
  tx
(** [default_tx ()] is the default value for type [tx] *)


(** {2 Protobuf Decoding} *)

val decode_tx : Pbrt.Decoder.t -> tx
(** [decode_tx decoder] decodes a [tx] value from [decoder] *)


(** {2 Protobuf Toding} *)

val encode_tx : tx -> Pbrt.Encoder.t -> unit
(** [encode_tx v encoder] encodes [v] with the given [encoder] *)


(** {2 Formatters} *)

val pp_tx : Format.formatter -> tx -> unit 
(** [pp_tx v] formats v *)
