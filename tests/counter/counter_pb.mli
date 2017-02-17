(** counter.proto Generated Types and Encoding *)


(** {2 Types} *)

type log = {
  counter_value : int;
  process_id : int;
}


(** {2 Default values} *)

val default_log : 
  ?counter_value:int ->
  ?process_id:int ->
  unit ->
  log
(** [default_log ()] is the default value for type [log] *)


(** {2 Protobuf Decoding} *)

val decode_log : Pbrt.Decoder.t -> log
(** [decode_log decoder] decodes a [log] value from [decoder] *)


(** {2 Protobuf Toding} *)

val encode_log : log -> Pbrt.Encoder.t -> unit
(** [encode_log v encoder] encodes [v] with the given [encoder] *)


(** {2 Formatters} *)

val pp_log : Format.formatter -> log -> unit 
(** [pp_log v] formats v *)
