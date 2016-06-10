(** raft_udp.proto Generated Types and Encoding *)

(** {2 Types} *)

type server_udp_configuration = {
  raft_id : int;
  inet4_address : string;
  raft_port : int;
  client_port : int;
}

type configuration = {
  raft_configuration : Raft_pb.configuration;
  servers_udp_configuration : server_udp_configuration list;
  compaction_period : float;
  log_record_directory : string;
  compaction_directory : string;
}

type log_entry = {
  request_id : string;
  data : bytes;
}

type client_request =
  | Add_log of log_entry

type client_response_add_log_not_aleader = {
  leader_id : int option;
}

type client_response =
  | Add_log_success
  | Add_log_validation_failure
  | Add_log_not_a_leader of client_response_add_log_not_aleader

type app_ipc_debug = {
  raft_server_id : int;
  debug_id : int;
}

type app_request_validate_logs = {
  log_entries : log_entry list;
}

type app_request_app_request_payload =
  | Validate_log of app_request_validate_logs
  | Commit_log of log_entry

and app_request = {
  app_request_debug_info : app_ipc_debug;
  app_request_payload : app_request_app_request_payload;
}

type app_response_validation_failure = {
  error_message : string;
  error_code : int;
}

type app_response_validation_result =
  | Success
  | Failure of app_response_validation_failure

and app_response_validation = {
  request_id : string;
  result : app_response_validation_result;
}

type app_response_validations = {
  validations : app_response_validation list;
}

type app_response_commit_log_ack = {
  request_id : string;
}

type app_response_app_response_payload =
  | Validations of app_response_validations
  | Commit_log_ack of app_response_commit_log_ack

and app_response = {
  app_response_debug_info : app_ipc_debug;
  app_response_payload : app_response_app_response_payload;
}


(** {2 Default values} *)

val default_server_udp_configuration : 
  ?raft_id:int ->
  ?inet4_address:string ->
  ?raft_port:int ->
  ?client_port:int ->
  unit ->
  server_udp_configuration
(** [default_server_udp_configuration ()] is the default value for type [server_udp_configuration] *)

val default_configuration : 
  ?raft_configuration:Raft_pb.configuration ->
  ?servers_udp_configuration:server_udp_configuration list ->
  ?compaction_period:float ->
  ?log_record_directory:string ->
  ?compaction_directory:string ->
  unit ->
  configuration
(** [default_configuration ()] is the default value for type [configuration] *)

val default_log_entry : 
  ?request_id:string ->
  ?data:bytes ->
  unit ->
  log_entry
(** [default_log_entry ()] is the default value for type [log_entry] *)

val default_client_request : unit -> client_request
(** [default_client_request ()] is the default value for type [client_request] *)

val default_client_response_add_log_not_aleader : 
  ?leader_id:int option ->
  unit ->
  client_response_add_log_not_aleader
(** [default_client_response_add_log_not_aleader ()] is the default value for type [client_response_add_log_not_aleader] *)

val default_client_response : unit -> client_response
(** [default_client_response ()] is the default value for type [client_response] *)

val default_app_ipc_debug : 
  ?raft_server_id:int ->
  ?debug_id:int ->
  unit ->
  app_ipc_debug
(** [default_app_ipc_debug ()] is the default value for type [app_ipc_debug] *)

val default_app_request_validate_logs : 
  ?log_entries:log_entry list ->
  unit ->
  app_request_validate_logs
(** [default_app_request_validate_logs ()] is the default value for type [app_request_validate_logs] *)

val default_app_request_app_request_payload : unit -> app_request_app_request_payload
(** [default_app_request_app_request_payload ()] is the default value for type [app_request_app_request_payload] *)

val default_app_request : 
  ?app_request_debug_info:app_ipc_debug ->
  ?app_request_payload:app_request_app_request_payload ->
  unit ->
  app_request
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
  ?request_id:string ->
  ?result:app_response_validation_result ->
  unit ->
  app_response_validation
(** [default_app_response_validation ()] is the default value for type [app_response_validation] *)

val default_app_response_validations : 
  ?validations:app_response_validation list ->
  unit ->
  app_response_validations
(** [default_app_response_validations ()] is the default value for type [app_response_validations] *)

val default_app_response_commit_log_ack : 
  ?request_id:string ->
  unit ->
  app_response_commit_log_ack
(** [default_app_response_commit_log_ack ()] is the default value for type [app_response_commit_log_ack] *)

val default_app_response_app_response_payload : unit -> app_response_app_response_payload
(** [default_app_response_app_response_payload ()] is the default value for type [app_response_app_response_payload] *)

val default_app_response : 
  ?app_response_debug_info:app_ipc_debug ->
  ?app_response_payload:app_response_app_response_payload ->
  unit ->
  app_response
(** [default_app_response ()] is the default value for type [app_response] *)


(** {2 Protobuf Decoding} *)

val decode_server_udp_configuration : Pbrt.Decoder.t -> server_udp_configuration
(** [decode_server_udp_configuration decoder] decodes a [server_udp_configuration] value from [decoder] *)

val decode_configuration : Pbrt.Decoder.t -> configuration
(** [decode_configuration decoder] decodes a [configuration] value from [decoder] *)

val decode_log_entry : Pbrt.Decoder.t -> log_entry
(** [decode_log_entry decoder] decodes a [log_entry] value from [decoder] *)

val decode_client_request : Pbrt.Decoder.t -> client_request
(** [decode_client_request decoder] decodes a [client_request] value from [decoder] *)

val decode_client_response_add_log_not_aleader : Pbrt.Decoder.t -> client_response_add_log_not_aleader
(** [decode_client_response_add_log_not_aleader decoder] decodes a [client_response_add_log_not_aleader] value from [decoder] *)

val decode_client_response : Pbrt.Decoder.t -> client_response
(** [decode_client_response decoder] decodes a [client_response] value from [decoder] *)

val decode_app_ipc_debug : Pbrt.Decoder.t -> app_ipc_debug
(** [decode_app_ipc_debug decoder] decodes a [app_ipc_debug] value from [decoder] *)

val decode_app_request_validate_logs : Pbrt.Decoder.t -> app_request_validate_logs
(** [decode_app_request_validate_logs decoder] decodes a [app_request_validate_logs] value from [decoder] *)

val decode_app_request_app_request_payload : Pbrt.Decoder.t -> app_request_app_request_payload
(** [decode_app_request_app_request_payload decoder] decodes a [app_request_app_request_payload] value from [decoder] *)

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

val decode_app_response_commit_log_ack : Pbrt.Decoder.t -> app_response_commit_log_ack
(** [decode_app_response_commit_log_ack decoder] decodes a [app_response_commit_log_ack] value from [decoder] *)

val decode_app_response_app_response_payload : Pbrt.Decoder.t -> app_response_app_response_payload
(** [decode_app_response_app_response_payload decoder] decodes a [app_response_app_response_payload] value from [decoder] *)

val decode_app_response : Pbrt.Decoder.t -> app_response
(** [decode_app_response decoder] decodes a [app_response] value from [decoder] *)


(** {2 Protobuf Toding} *)

val encode_server_udp_configuration : server_udp_configuration -> Pbrt.Encoder.t -> unit
(** [encode_server_udp_configuration v encoder] encodes [v] with the given [encoder] *)

val encode_configuration : configuration -> Pbrt.Encoder.t -> unit
(** [encode_configuration v encoder] encodes [v] with the given [encoder] *)

val encode_log_entry : log_entry -> Pbrt.Encoder.t -> unit
(** [encode_log_entry v encoder] encodes [v] with the given [encoder] *)

val encode_client_request : client_request -> Pbrt.Encoder.t -> unit
(** [encode_client_request v encoder] encodes [v] with the given [encoder] *)

val encode_client_response_add_log_not_aleader : client_response_add_log_not_aleader -> Pbrt.Encoder.t -> unit
(** [encode_client_response_add_log_not_aleader v encoder] encodes [v] with the given [encoder] *)

val encode_client_response : client_response -> Pbrt.Encoder.t -> unit
(** [encode_client_response v encoder] encodes [v] with the given [encoder] *)

val encode_app_ipc_debug : app_ipc_debug -> Pbrt.Encoder.t -> unit
(** [encode_app_ipc_debug v encoder] encodes [v] with the given [encoder] *)

val encode_app_request_validate_logs : app_request_validate_logs -> Pbrt.Encoder.t -> unit
(** [encode_app_request_validate_logs v encoder] encodes [v] with the given [encoder] *)

val encode_app_request_app_request_payload : app_request_app_request_payload -> Pbrt.Encoder.t -> unit
(** [encode_app_request_app_request_payload v encoder] encodes [v] with the given [encoder] *)

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

val encode_app_response_commit_log_ack : app_response_commit_log_ack -> Pbrt.Encoder.t -> unit
(** [encode_app_response_commit_log_ack v encoder] encodes [v] with the given [encoder] *)

val encode_app_response_app_response_payload : app_response_app_response_payload -> Pbrt.Encoder.t -> unit
(** [encode_app_response_app_response_payload v encoder] encodes [v] with the given [encoder] *)

val encode_app_response : app_response -> Pbrt.Encoder.t -> unit
(** [encode_app_response v encoder] encodes [v] with the given [encoder] *)


(** {2 Formatters} *)

val pp_server_udp_configuration : Format.formatter -> server_udp_configuration -> unit 
(** [pp_server_udp_configuration v] formats v] *)

val pp_configuration : Format.formatter -> configuration -> unit 
(** [pp_configuration v] formats v] *)

val pp_log_entry : Format.formatter -> log_entry -> unit 
(** [pp_log_entry v] formats v] *)

val pp_client_request : Format.formatter -> client_request -> unit 
(** [pp_client_request v] formats v] *)

val pp_client_response_add_log_not_aleader : Format.formatter -> client_response_add_log_not_aleader -> unit 
(** [pp_client_response_add_log_not_aleader v] formats v] *)

val pp_client_response : Format.formatter -> client_response -> unit 
(** [pp_client_response v] formats v] *)

val pp_app_ipc_debug : Format.formatter -> app_ipc_debug -> unit 
(** [pp_app_ipc_debug v] formats v] *)

val pp_app_request_validate_logs : Format.formatter -> app_request_validate_logs -> unit 
(** [pp_app_request_validate_logs v] formats v] *)

val pp_app_request_app_request_payload : Format.formatter -> app_request_app_request_payload -> unit 
(** [pp_app_request_app_request_payload v] formats v] *)

val pp_app_request : Format.formatter -> app_request -> unit 
(** [pp_app_request v] formats v] *)

val pp_app_response_validation_failure : Format.formatter -> app_response_validation_failure -> unit 
(** [pp_app_response_validation_failure v] formats v] *)

val pp_app_response_validation_result : Format.formatter -> app_response_validation_result -> unit 
(** [pp_app_response_validation_result v] formats v] *)

val pp_app_response_validation : Format.formatter -> app_response_validation -> unit 
(** [pp_app_response_validation v] formats v] *)

val pp_app_response_validations : Format.formatter -> app_response_validations -> unit 
(** [pp_app_response_validations v] formats v] *)

val pp_app_response_commit_log_ack : Format.formatter -> app_response_commit_log_ack -> unit 
(** [pp_app_response_commit_log_ack v] formats v] *)

val pp_app_response_app_response_payload : Format.formatter -> app_response_app_response_payload -> unit 
(** [pp_app_response_app_response_payload v] formats v] *)

val pp_app_response : Format.formatter -> app_response -> unit 
(** [pp_app_response v] formats v] *)
