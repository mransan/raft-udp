(** raft_clt.proto Generated Types and Encoding *)


(** {2 Types} *)

type client_request =
  | Add_tx of Raft_com_pb.tx

type client_response_add_log_not_aleader = {
  leader_id : int option;
}

type client_response =
  | Add_log_success
  | Add_log_validation_failure
  | Add_log_not_a_leader of client_response_add_log_not_aleader


(** {2 Default values} *)

val default_client_request : unit -> client_request
(** [default_client_request ()] is the default value for type [client_request] *)

val default_client_response_add_log_not_aleader : 
  ?leader_id:int option ->
  unit ->
  client_response_add_log_not_aleader
(** [default_client_response_add_log_not_aleader ()] is the default value for type [client_response_add_log_not_aleader] *)

val default_client_response : unit -> client_response
(** [default_client_response ()] is the default value for type [client_response] *)


(** {2 Protobuf Decoding} *)

val decode_client_request : Pbrt.Decoder.t -> client_request
(** [decode_client_request decoder] decodes a [client_request] value from [decoder] *)

val decode_client_response_add_log_not_aleader : Pbrt.Decoder.t -> client_response_add_log_not_aleader
(** [decode_client_response_add_log_not_aleader decoder] decodes a [client_response_add_log_not_aleader] value from [decoder] *)

val decode_client_response : Pbrt.Decoder.t -> client_response
(** [decode_client_response decoder] decodes a [client_response] value from [decoder] *)


(** {2 Protobuf Toding} *)

val encode_client_request : client_request -> Pbrt.Encoder.t -> unit
(** [encode_client_request v encoder] encodes [v] with the given [encoder] *)

val encode_client_response_add_log_not_aleader : client_response_add_log_not_aleader -> Pbrt.Encoder.t -> unit
(** [encode_client_response_add_log_not_aleader v encoder] encodes [v] with the given [encoder] *)

val encode_client_response : client_response -> Pbrt.Encoder.t -> unit
(** [encode_client_response v encoder] encodes [v] with the given [encoder] *)


(** {2 Formatters} *)

val pp_client_request : Format.formatter -> client_request -> unit 
(** [pp_client_request v] formats v *)

val pp_client_response_add_log_not_aleader : Format.formatter -> client_response_add_log_not_aleader -> unit 
(** [pp_client_response_add_log_not_aleader v] formats v *)

val pp_client_response : Format.formatter -> client_response -> unit 
(** [pp_client_response v] formats v *)
