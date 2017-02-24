(** counter.proto Generated Types and Encoding *)


(** {2 Types} *)

type app_data = {
  increment : int;
  process_id : int;
}

type app_result = {
  from : int;
  to_ : int option;
}


(** {2 Default values} *)

val default_app_data : 
  ?increment:int ->
  ?process_id:int ->
  unit ->
  app_data
(** [default_app_data ()] is the default value for type [app_data] *)

val default_app_result : 
  ?from:int ->
  ?to_:int option ->
  unit ->
  app_result
(** [default_app_result ()] is the default value for type [app_result] *)


(** {2 Protobuf Decoding} *)

val decode_app_data : Pbrt.Decoder.t -> app_data
(** [decode_app_data decoder] decodes a [app_data] value from [decoder] *)

val decode_app_result : Pbrt.Decoder.t -> app_result
(** [decode_app_result decoder] decodes a [app_result] value from [decoder] *)


(** {2 Protobuf Toding} *)

val encode_app_data : app_data -> Pbrt.Encoder.t -> unit
(** [encode_app_data v encoder] encodes [v] with the given [encoder] *)

val encode_app_result : app_result -> Pbrt.Encoder.t -> unit
(** [encode_app_result v encoder] encodes [v] with the given [encoder] *)


(** {2 Formatters} *)

val pp_app_data : Format.formatter -> app_data -> unit 
(** [pp_app_data v] formats v *)

val pp_app_result : Format.formatter -> app_result -> unit 
(** [pp_app_result v] formats v *)
