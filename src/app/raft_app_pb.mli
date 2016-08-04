(** raft_app.proto Generated Types and Encoding *)


(** {2 Types} *)

type app_request_txs = {
  txs : Raft_com_pb.tx list;
}

type app_request =
  | Commit_txs of app_request_txs

type app_response_validation_failure = {
  error_message : string;
  error_code : int;
}

type app_response_validation_result =
  | Validation_success
  | Validation_failure of app_response_validation_failure

and app_response_validation = {
  tx_id : string;
  result : app_response_validation_result;
}

type app_response_validations = {
  validations : app_response_validation list;
}

type app_response =
  | Committed_txs of app_response_validations


(** {2 Default values} *)

val default_app_request_txs : 
  ?txs:Raft_com_pb.tx list ->
  unit ->
  app_request_txs
(** [default_app_request_txs ()] is the default value for type [app_request_txs] *)

val default_app_request : unit -> app_request
(** [default_app_request ()] is the default value for type [app_request] *)

val default_app_response_validation_failure : 
  ?error_message:string ->
  ?error_code:int ->
  unit ->
  app_response_validation_failure
(** [default_app_response_validation_failure ()] is the default value for type [app_response_validation_failure] *)

val default_app_response_validation_result : unit -> app_response_validation_result
(** [default_app_response_validation_result ()] is the default value for type [app_response_validation_result] *)

val default_app_response_validation : 
  ?tx_id:string ->
  ?result:app_response_validation_result ->
  unit ->
  app_response_validation
(** [default_app_response_validation ()] is the default value for type [app_response_validation] *)

val default_app_response_validations : 
  ?validations:app_response_validation list ->
  unit ->
  app_response_validations
(** [default_app_response_validations ()] is the default value for type [app_response_validations] *)

val default_app_response : unit -> app_response
(** [default_app_response ()] is the default value for type [app_response] *)


(** {2 Protobuf Decoding} *)

val decode_app_request_txs : Pbrt.Decoder.t -> app_request_txs
(** [decode_app_request_txs decoder] decodes a [app_request_txs] value from [decoder] *)

val decode_app_request : Pbrt.Decoder.t -> app_request
(** [decode_app_request decoder] decodes a [app_request] value from [decoder] *)

val decode_app_response_validation_failure : Pbrt.Decoder.t -> app_response_validation_failure
(** [decode_app_response_validation_failure decoder] decodes a [app_response_validation_failure] value from [decoder] *)

val decode_app_response_validation_result : Pbrt.Decoder.t -> app_response_validation_result
(** [decode_app_response_validation_result decoder] decodes a [app_response_validation_result] value from [decoder] *)

val decode_app_response_validation : Pbrt.Decoder.t -> app_response_validation
(** [decode_app_response_validation decoder] decodes a [app_response_validation] value from [decoder] *)

val decode_app_response_validations : Pbrt.Decoder.t -> app_response_validations
(** [decode_app_response_validations decoder] decodes a [app_response_validations] value from [decoder] *)

val decode_app_response : Pbrt.Decoder.t -> app_response
(** [decode_app_response decoder] decodes a [app_response] value from [decoder] *)


(** {2 Protobuf Toding} *)

val encode_app_request_txs : app_request_txs -> Pbrt.Encoder.t -> unit
(** [encode_app_request_txs v encoder] encodes [v] with the given [encoder] *)

val encode_app_request : app_request -> Pbrt.Encoder.t -> unit
(** [encode_app_request v encoder] encodes [v] with the given [encoder] *)

val encode_app_response_validation_failure : app_response_validation_failure -> Pbrt.Encoder.t -> unit
(** [encode_app_response_validation_failure v encoder] encodes [v] with the given [encoder] *)

val encode_app_response_validation_result : app_response_validation_result -> Pbrt.Encoder.t -> unit
(** [encode_app_response_validation_result v encoder] encodes [v] with the given [encoder] *)

val encode_app_response_validation : app_response_validation -> Pbrt.Encoder.t -> unit
(** [encode_app_response_validation v encoder] encodes [v] with the given [encoder] *)

val encode_app_response_validations : app_response_validations -> Pbrt.Encoder.t -> unit
(** [encode_app_response_validations v encoder] encodes [v] with the given [encoder] *)

val encode_app_response : app_response -> Pbrt.Encoder.t -> unit
(** [encode_app_response v encoder] encodes [v] with the given [encoder] *)


(** {2 Formatters} *)

val pp_app_request_txs : Format.formatter -> app_request_txs -> unit 
(** [pp_app_request_txs v] formats v *)

val pp_app_request : Format.formatter -> app_request -> unit 
(** [pp_app_request v] formats v *)

val pp_app_response_validation_failure : Format.formatter -> app_response_validation_failure -> unit 
(** [pp_app_response_validation_failure v] formats v *)

val pp_app_response_validation_result : Format.formatter -> app_response_validation_result -> unit 
(** [pp_app_response_validation_result v] formats v *)

val pp_app_response_validation : Format.formatter -> app_response_validation -> unit 
(** [pp_app_response_validation v] formats v *)

val pp_app_response_validations : Format.formatter -> app_response_validations -> unit 
(** [pp_app_response_validations v] formats v *)

val pp_app_response : Format.formatter -> app_response -> unit 
(** [pp_app_response v] formats v *)
