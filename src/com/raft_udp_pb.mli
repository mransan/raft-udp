(** raft_udp.proto Generated Types and Encoding *)


(** {2 Types} *)

type server_ipc_configuration = {
  raft_id : int;
  inet4_address : string;
  raft_port : int;
  client_port : int;
  app_server_port : int;
}

type disk_backup_configuration = {
  compaction_period : float;
  log_record_directory : string;
  compaction_directory : string;
}

type configuration = {
  raft_configuration : Raft_pb.configuration;
  servers_ipc_configuration : server_ipc_configuration list;
  disk_backup : disk_backup_configuration;
}


(** {2 Default values} *)

val default_server_ipc_configuration : 
  ?raft_id:int ->
  ?inet4_address:string ->
  ?raft_port:int ->
  ?client_port:int ->
  ?app_server_port:int ->
  unit ->
  server_ipc_configuration
(** [default_server_ipc_configuration ()] is the default value for type [server_ipc_configuration] *)

val default_disk_backup_configuration : 
  ?compaction_period:float ->
  ?log_record_directory:string ->
  ?compaction_directory:string ->
  unit ->
  disk_backup_configuration
(** [default_disk_backup_configuration ()] is the default value for type [disk_backup_configuration] *)

val default_configuration : 
  ?raft_configuration:Raft_pb.configuration ->
  ?servers_ipc_configuration:server_ipc_configuration list ->
  ?disk_backup:disk_backup_configuration ->
  unit ->
  configuration
(** [default_configuration ()] is the default value for type [configuration] *)


(** {2 Protobuf Decoding} *)

val decode_server_ipc_configuration : Pbrt.Decoder.t -> server_ipc_configuration
(** [decode_server_ipc_configuration decoder] decodes a [server_ipc_configuration] value from [decoder] *)

val decode_disk_backup_configuration : Pbrt.Decoder.t -> disk_backup_configuration
(** [decode_disk_backup_configuration decoder] decodes a [disk_backup_configuration] value from [decoder] *)

val decode_configuration : Pbrt.Decoder.t -> configuration
(** [decode_configuration decoder] decodes a [configuration] value from [decoder] *)


(** {2 Protobuf Toding} *)

val encode_server_ipc_configuration : server_ipc_configuration -> Pbrt.Encoder.t -> unit
(** [encode_server_ipc_configuration v encoder] encodes [v] with the given [encoder] *)

val encode_disk_backup_configuration : disk_backup_configuration -> Pbrt.Encoder.t -> unit
(** [encode_disk_backup_configuration v encoder] encodes [v] with the given [encoder] *)

val encode_configuration : configuration -> Pbrt.Encoder.t -> unit
(** [encode_configuration v encoder] encodes [v] with the given [encoder] *)


(** {2 Formatters} *)

val pp_server_ipc_configuration : Format.formatter -> server_ipc_configuration -> unit 
(** [pp_server_ipc_configuration v] formats v *)

val pp_disk_backup_configuration : Format.formatter -> disk_backup_configuration -> unit 
(** [pp_disk_backup_configuration v] formats v *)

val pp_configuration : Format.formatter -> configuration -> unit 
(** [pp_configuration v] formats v *)
