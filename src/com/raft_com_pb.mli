(** raft_com.proto Generated Types and Encoding *)


(** {2 Types} *)

type tx = {
  tx_id : string;
  tx_data : bytes;
}

type client_request =
  | Add_tx of tx

type client_response_add_log_not_aleader = {
  leader_id : int option;
}

type client_response =
  | Add_log_success
  | Add_log_validation_failure
  | Add_log_not_a_leader of client_response_add_log_not_aleader

type app_request_validate_txs = {
  txs : tx list;
}

type app_request =
  | Validate_txs of app_request_validate_txs
  | Commit_tx of tx

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

type app_response_commit_tx_ack = {
  tx_id : string;
}

type app_response =
  | Validations of app_response_validations
  | Commit_tx_ack of app_response_commit_tx_ack


(** {2 Default values} *)

val default_tx : 
  ?tx_id:string ->
  ?tx_data:bytes ->
  unit ->
  tx
(** [default_tx ()] is the default value for type [tx] *)

val default_client_request : unit -> client_request
(** [default_client_request ()] is the default value for type [client_request] *)

val default_client_response_add_log_not_aleader : 
  ?leader_id:int option ->
  unit ->
  client_response_add_log_not_aleader
(** [default_client_response_add_log_not_aleader ()] is the default value for type [client_response_add_log_not_aleader] *)

val default_client_response : unit -> client_response
(** [default_client_response ()] is the default value for type [client_response] *)

val default_app_request_validate_txs : 
  ?txs:tx list ->
  unit ->
  app_request_validate_txs
(** [default_app_request_validate_txs ()] is the default value for type [app_request_validate_txs] *)

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

val default_app_response_commit_tx_ack : 
  ?tx_id:string ->
  unit ->
  app_response_commit_tx_ack
(** [default_app_response_commit_tx_ack ()] is the default value for type [app_response_commit_tx_ack] *)

val default_app_response : unit -> app_response
(** [default_app_response ()] is the default value for type [app_response] *)


(** {2 Protobuf Decoding} *)

val decode_tx : Pbrt.Decoder.t -> tx
(** [decode_tx decoder] decodes a [tx] value from [decoder] *)

val decode_client_request : Pbrt.Decoder.t -> client_request
(** [decode_client_request decoder] decodes a [client_request] value from [decoder] *)

val decode_client_response_add_log_not_aleader : Pbrt.Decoder.t -> client_response_add_log_not_aleader
(** [decode_client_response_add_log_not_aleader decoder] decodes a [client_response_add_log_not_aleader] value from [decoder] *)

val decode_client_response : Pbrt.Decoder.t -> client_response
(** [decode_client_response decoder] decodes a [client_response] value from [decoder] *)

val decode_app_request_validate_txs : Pbrt.Decoder.t -> app_request_validate_txs
(** [decode_app_request_validate_txs decoder] decodes a [app_request_validate_txs] value from [decoder] *)

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

val decode_app_response_commit_tx_ack : Pbrt.Decoder.t -> app_response_commit_tx_ack
(** [decode_app_response_commit_tx_ack decoder] decodes a [app_response_commit_tx_ack] value from [decoder] *)

val decode_app_response : Pbrt.Decoder.t -> app_response
(** [decode_app_response decoder] decodes a [app_response] value from [decoder] *)


(** {2 Protobuf Toding} *)

val encode_tx : tx -> Pbrt.Encoder.t -> unit
(** [encode_tx v encoder] encodes [v] with the given [encoder] *)

val encode_client_request : client_request -> Pbrt.Encoder.t -> unit
(** [encode_client_request v encoder] encodes [v] with the given [encoder] *)

val encode_client_response_add_log_not_aleader : client_response_add_log_not_aleader -> Pbrt.Encoder.t -> unit
(** [encode_client_response_add_log_not_aleader v encoder] encodes [v] with the given [encoder] *)

val encode_client_response : client_response -> Pbrt.Encoder.t -> unit
(** [encode_client_response v encoder] encodes [v] with the given [encoder] *)

val encode_app_request_validate_txs : app_request_validate_txs -> Pbrt.Encoder.t -> unit
(** [encode_app_request_validate_txs v encoder] encodes [v] with the given [encoder] *)

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

val encode_app_response_commit_tx_ack : app_response_commit_tx_ack -> Pbrt.Encoder.t -> unit
(** [encode_app_response_commit_tx_ack v encoder] encodes [v] with the given [encoder] *)

val encode_app_response : app_response -> Pbrt.Encoder.t -> unit
(** [encode_app_response v encoder] encodes [v] with the given [encoder] *)


(** {2 Formatters} *)

val pp_tx : Format.formatter -> tx -> unit 
(** [pp_tx v] formats v *)

val pp_client_request : Format.formatter -> client_request -> unit 
(** [pp_client_request v] formats v *)

val pp_client_response_add_log_not_aleader : Format.formatter -> client_response_add_log_not_aleader -> unit 
(** [pp_client_response_add_log_not_aleader v] formats v *)

val pp_client_response : Format.formatter -> client_response -> unit 
(** [pp_client_response v] formats v *)

val pp_app_request_validate_txs : Format.formatter -> app_request_validate_txs -> unit 
(** [pp_app_request_validate_txs v] formats v *)

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

val pp_app_response_commit_tx_ack : Format.formatter -> app_response_commit_tx_ack -> unit 
(** [pp_app_response_commit_tx_ack v] formats v *)

val pp_app_response : Format.formatter -> app_response -> unit 
(** [pp_app_response v] formats v *)
