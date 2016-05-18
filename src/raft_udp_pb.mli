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
}

type client_request_add_log = {
  request_id : string;
  data : bytes;
}

type client_request =
  | Ping
  | Add_log of client_request_add_log

type client_response_not_aleader = {
  leader_id : int;
}

type client_response =
  | Success
  | Replication_failure
  | Not_a_leader of client_response_not_aleader


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
  unit ->
  configuration
(** [default_configuration ()] is the default value for type [configuration] *)

val default_client_request_add_log : 
  ?request_id:string ->
  ?data:bytes ->
  unit ->
  client_request_add_log
(** [default_client_request_add_log ()] is the default value for type [client_request_add_log] *)

val default_client_request : unit -> client_request
(** [default_client_request ()] is the default value for type [client_request] *)

val default_client_response_not_aleader : 
  ?leader_id:int ->
  unit ->
  client_response_not_aleader
(** [default_client_response_not_aleader ()] is the default value for type [client_response_not_aleader] *)

val default_client_response : unit -> client_response
(** [default_client_response ()] is the default value for type [client_response] *)


(** {2 Protobuf Decoding} *)

val decode_server_udp_configuration : Pbrt.Decoder.t -> server_udp_configuration
(** [decode_server_udp_configuration decoder] decodes a [server_udp_configuration] value from [decoder] *)

val decode_configuration : Pbrt.Decoder.t -> configuration
(** [decode_configuration decoder] decodes a [configuration] value from [decoder] *)

val decode_client_request_add_log : Pbrt.Decoder.t -> client_request_add_log
(** [decode_client_request_add_log decoder] decodes a [client_request_add_log] value from [decoder] *)

val decode_client_request : Pbrt.Decoder.t -> client_request
(** [decode_client_request decoder] decodes a [client_request] value from [decoder] *)

val decode_client_response_not_aleader : Pbrt.Decoder.t -> client_response_not_aleader
(** [decode_client_response_not_aleader decoder] decodes a [client_response_not_aleader] value from [decoder] *)

val decode_client_response : Pbrt.Decoder.t -> client_response
(** [decode_client_response decoder] decodes a [client_response] value from [decoder] *)


(** {2 Protobuf Toding} *)

val encode_server_udp_configuration : server_udp_configuration -> Pbrt.Encoder.t -> unit
(** [encode_server_udp_configuration v encoder] encodes [v] with the given [encoder] *)

val encode_configuration : configuration -> Pbrt.Encoder.t -> unit
(** [encode_configuration v encoder] encodes [v] with the given [encoder] *)

val encode_client_request_add_log : client_request_add_log -> Pbrt.Encoder.t -> unit
(** [encode_client_request_add_log v encoder] encodes [v] with the given [encoder] *)

val encode_client_request : client_request -> Pbrt.Encoder.t -> unit
(** [encode_client_request v encoder] encodes [v] with the given [encoder] *)

val encode_client_response_not_aleader : client_response_not_aleader -> Pbrt.Encoder.t -> unit
(** [encode_client_response_not_aleader v encoder] encodes [v] with the given [encoder] *)

val encode_client_response : client_response -> Pbrt.Encoder.t -> unit
(** [encode_client_response v encoder] encodes [v] with the given [encoder] *)


(** {2 Formatters} *)

val pp_server_udp_configuration : Format.formatter -> server_udp_configuration -> unit 
(** [pp_server_udp_configuration v] formats v] *)

val pp_configuration : Format.formatter -> configuration -> unit 
(** [pp_configuration v] formats v] *)

val pp_client_request_add_log : Format.formatter -> client_request_add_log -> unit 
(** [pp_client_request_add_log v] formats v] *)

val pp_client_request : Format.formatter -> client_request -> unit 
(** [pp_client_request v] formats v] *)

val pp_client_response_not_aleader : Format.formatter -> client_response_not_aleader -> unit 
(** [pp_client_response_not_aleader v] formats v] *)

val pp_client_response : Format.formatter -> client_response -> unit 
(** [pp_client_response v] formats v] *)
