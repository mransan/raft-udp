(** raft_com.proto Generated Types and Encoding *)


(** {2 Types} *)

type client_log_entry = {
  client_log_id : string;
  client_log_data : bytes;
}

type client_request =
  | Add_log_entry of client_log_entry

type client_response_result = {
  client_log_id : string;
  client_log_result_data : bytes option;
}

type client_response_not_aleader = {
  leader_id : int option;
}

type client_response =
  | Add_log_result of client_response_result
  | Add_log_not_a_leader of client_response_not_aleader

type app_request_add_log_entries = {
  log_entries : Raft_pb.log_entry list;
}

type app_request =
  | Add_log_entries of app_request_add_log_entries

type app_response_result = {
  index : int;
  id : string;
  result_data : bytes option;
}

type app_response_results = {
  results : app_response_result list;
}

type app_response =
  | Add_log_results of app_response_results


(** {2 Default values} *)

val default_client_log_entry : 
  ?client_log_id:string ->
  ?client_log_data:bytes ->
  unit ->
  client_log_entry
(** [default_client_log_entry ()] is the default value for type [client_log_entry] *)

val default_client_request : unit -> client_request
(** [default_client_request ()] is the default value for type [client_request] *)

val default_client_response_result : 
  ?client_log_id:string ->
  ?client_log_result_data:bytes option ->
  unit ->
  client_response_result
(** [default_client_response_result ()] is the default value for type [client_response_result] *)

val default_client_response_not_aleader : 
  ?leader_id:int option ->
  unit ->
  client_response_not_aleader
(** [default_client_response_not_aleader ()] is the default value for type [client_response_not_aleader] *)

val default_client_response : unit -> client_response
(** [default_client_response ()] is the default value for type [client_response] *)

val default_app_request_add_log_entries : 
  ?log_entries:Raft_pb.log_entry list ->
  unit ->
  app_request_add_log_entries
(** [default_app_request_add_log_entries ()] is the default value for type [app_request_add_log_entries] *)

val default_app_request : unit -> app_request
(** [default_app_request ()] is the default value for type [app_request] *)

val default_app_response_result : 
  ?index:int ->
  ?id:string ->
  ?result_data:bytes option ->
  unit ->
  app_response_result
(** [default_app_response_result ()] is the default value for type [app_response_result] *)

val default_app_response_results : 
  ?results:app_response_result list ->
  unit ->
  app_response_results
(** [default_app_response_results ()] is the default value for type [app_response_results] *)

val default_app_response : unit -> app_response
(** [default_app_response ()] is the default value for type [app_response] *)


(** {2 Protobuf Decoding} *)

val decode_client_log_entry : Pbrt.Decoder.t -> client_log_entry
(** [decode_client_log_entry decoder] decodes a [client_log_entry] value from [decoder] *)

val decode_client_request : Pbrt.Decoder.t -> client_request
(** [decode_client_request decoder] decodes a [client_request] value from [decoder] *)

val decode_client_response_result : Pbrt.Decoder.t -> client_response_result
(** [decode_client_response_result decoder] decodes a [client_response_result] value from [decoder] *)

val decode_client_response_not_aleader : Pbrt.Decoder.t -> client_response_not_aleader
(** [decode_client_response_not_aleader decoder] decodes a [client_response_not_aleader] value from [decoder] *)

val decode_client_response : Pbrt.Decoder.t -> client_response
(** [decode_client_response decoder] decodes a [client_response] value from [decoder] *)

val decode_app_request_add_log_entries : Pbrt.Decoder.t -> app_request_add_log_entries
(** [decode_app_request_add_log_entries decoder] decodes a [app_request_add_log_entries] value from [decoder] *)

val decode_app_request : Pbrt.Decoder.t -> app_request
(** [decode_app_request decoder] decodes a [app_request] value from [decoder] *)

val decode_app_response_result : Pbrt.Decoder.t -> app_response_result
(** [decode_app_response_result decoder] decodes a [app_response_result] value from [decoder] *)

val decode_app_response_results : Pbrt.Decoder.t -> app_response_results
(** [decode_app_response_results decoder] decodes a [app_response_results] value from [decoder] *)

val decode_app_response : Pbrt.Decoder.t -> app_response
(** [decode_app_response decoder] decodes a [app_response] value from [decoder] *)


(** {2 Protobuf Toding} *)

val encode_client_log_entry : client_log_entry -> Pbrt.Encoder.t -> unit
(** [encode_client_log_entry v encoder] encodes [v] with the given [encoder] *)

val encode_client_request : client_request -> Pbrt.Encoder.t -> unit
(** [encode_client_request v encoder] encodes [v] with the given [encoder] *)

val encode_client_response_result : client_response_result -> Pbrt.Encoder.t -> unit
(** [encode_client_response_result v encoder] encodes [v] with the given [encoder] *)

val encode_client_response_not_aleader : client_response_not_aleader -> Pbrt.Encoder.t -> unit
(** [encode_client_response_not_aleader v encoder] encodes [v] with the given [encoder] *)

val encode_client_response : client_response -> Pbrt.Encoder.t -> unit
(** [encode_client_response v encoder] encodes [v] with the given [encoder] *)

val encode_app_request_add_log_entries : app_request_add_log_entries -> Pbrt.Encoder.t -> unit
(** [encode_app_request_add_log_entries v encoder] encodes [v] with the given [encoder] *)

val encode_app_request : app_request -> Pbrt.Encoder.t -> unit
(** [encode_app_request v encoder] encodes [v] with the given [encoder] *)

val encode_app_response_result : app_response_result -> Pbrt.Encoder.t -> unit
(** [encode_app_response_result v encoder] encodes [v] with the given [encoder] *)

val encode_app_response_results : app_response_results -> Pbrt.Encoder.t -> unit
(** [encode_app_response_results v encoder] encodes [v] with the given [encoder] *)

val encode_app_response : app_response -> Pbrt.Encoder.t -> unit
(** [encode_app_response v encoder] encodes [v] with the given [encoder] *)


(** {2 Formatters} *)

val pp_client_log_entry : Format.formatter -> client_log_entry -> unit 
(** [pp_client_log_entry v] formats v *)

val pp_client_request : Format.formatter -> client_request -> unit 
(** [pp_client_request v] formats v *)

val pp_client_response_result : Format.formatter -> client_response_result -> unit 
(** [pp_client_response_result v] formats v *)

val pp_client_response_not_aleader : Format.formatter -> client_response_not_aleader -> unit 
(** [pp_client_response_not_aleader v] formats v *)

val pp_client_response : Format.formatter -> client_response -> unit 
(** [pp_client_response v] formats v *)

val pp_app_request_add_log_entries : Format.formatter -> app_request_add_log_entries -> unit 
(** [pp_app_request_add_log_entries v] formats v *)

val pp_app_request : Format.formatter -> app_request -> unit 
(** [pp_app_request v] formats v *)

val pp_app_response_result : Format.formatter -> app_response_result -> unit 
(** [pp_app_response_result v] formats v *)

val pp_app_response_results : Format.formatter -> app_response_results -> unit 
(** [pp_app_response_results v] formats v *)

val pp_app_response : Format.formatter -> app_response -> unit 
(** [pp_app_response v] formats v *)
