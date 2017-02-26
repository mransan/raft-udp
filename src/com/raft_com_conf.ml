
module RTypes = Raft_types

type server_ipc_configuration = {
  raft_id : int;
  inet4_address : string;
  raft_port : int;
  client_port : int;
}

type disk_backup_configuration = {
  log_record_directory : string;
}

type t = {
  raft_configuration : Raft_types.configuration;
  servers_ipc_configuration : server_ipc_configuration list;
  disk_backup : disk_backup_configuration;
  app_server_port : int list;
}

let default_configuration () = {
  raft_configuration = RTypes.({
    nb_of_server = 3;
    election_timeout = 0.50;
    election_timeout_range = 0.2;
    hearbeat_timeout = 0.1;
    max_nb_logs_per_message = 50;
    max_log_size = {
      Raft_log.upper_bound = 200_000; 
      Raft_log.lower_bound = 100_000; 
    };
  });

  servers_ipc_configuration = [
    {raft_id = 0; inet4_address = "127.0.0.1"; raft_port = 34765; client_port = 34865};
    {raft_id = 1; inet4_address = "127.0.0.1"; raft_port = 34766; client_port = 34866};
    {raft_id = 2; inet4_address = "127.0.0.1"; raft_port = 34767; client_port = 34867};
    (*
    {raft_id = 3; inet4_address = "127.0.0.1"; raft_port = 34768; client_port = 34868};
    {raft_id = 4; inet4_address = "127.0.0.1"; raft_port = 34769; client_port = 34869};
    {raft_id = 5; inet4_address = "127.0.0.1"; raft_port = 34760; client_port = 34860};
    {raft_id = 6; inet4_address = "127.0.0.1"; raft_port = 34761; client_port = 34861};
    {raft_id = 7; inet4_address = "127.0.0.1"; raft_port = 34762; client_port = 34862};
    {raft_id = 8; inet4_address = "127.0.0.1"; raft_port = 34763; client_port = 34863};
    *)
  ];

  disk_backup = {
    log_record_directory = "/tmp/";
  };

  app_server_port = [40_000; 40_001; 40_002];
}

let sockaddr_of_server_config which server_ipc_conf = 
  let {inet4_address; raft_port; client_port; _ } = server_ipc_conf in 
  let port = match which with
    | `Client  -> client_port 
    | `Raft    -> raft_port
  in 
  Unix.ADDR_INET (Unix.inet_addr_of_string inet4_address, port)

let sockaddr_of_server_id which configuration server_id =
  let is_server {raft_id; _ } =
    raft_id = server_id
  in
  match List.find is_server configuration.servers_ipc_configuration with
  | server_config -> Some (sockaddr_of_server_config which server_config)
  | exception Not_found -> None
