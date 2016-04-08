(** raft_udp.proto Generated Types and Encoding *)

(** {2 Types} *)

type server_udp_configuration = {
  raft_id : int;
  inet4_address : string;
  port : int;
}

type configuration = {
  raft_configuration : Raft_pb.configuration;
  servers_udp_configuration : server_udp_configuration list;
}


(** {2 Default values} *)

val default_server_udp_configuration : 
  ?raft_id:int ->
  ?inet4_address:string ->
  ?port:int ->
  unit ->
  server_udp_configuration
(** [default_server_udp_configuration ()] is the default value for type [server_udp_configuration] *)

val default_configuration : 
  ?raft_configuration:Raft_pb.configuration ->
  ?servers_udp_configuration:server_udp_configuration list ->
  unit ->
  configuration
(** [default_configuration ()] is the default value for type [configuration] *)


(** {2 Protobuf Decoding} *)

val decode_server_udp_configuration : Pbrt.Decoder.t -> server_udp_configuration
(** [decode_server_udp_configuration decoder] decodes a [server_udp_configuration] value from [decoder] *)

val decode_configuration : Pbrt.Decoder.t -> configuration
(** [decode_configuration decoder] decodes a [configuration] value from [decoder] *)


(** {2 Protobuf Toding} *)

val encode_server_udp_configuration : server_udp_configuration -> Pbrt.Encoder.t -> unit
(** [encode_server_udp_configuration v encoder] encodes [v] with the given [encoder] *)

val encode_configuration : configuration -> Pbrt.Encoder.t -> unit
(** [encode_configuration v encoder] encodes [v] with the given [encoder] *)


(** {2 Formatters} *)

val pp_server_udp_configuration : Format.formatter -> server_udp_configuration -> unit 
(** [pp_server_udp_configuration v] formats v] *)

val pp_configuration : Format.formatter -> configuration -> unit 
(** [pp_configuration v] formats v] *)
