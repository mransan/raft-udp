(** raft_udp.proto Generated Types and Encoding *)

(** {2 Types} *)

type server_ipc_configuration = {
  raft_id : int;
  inet4_address : string;
  raft_port : int;
  client_port : int;
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
  app_server_port : int;
}
