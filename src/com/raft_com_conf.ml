module RTypes = Raft_types

type server_ipc_configuration = {
  raft_id : int;
  inet4_address : string;
  raft_port : int;
  client_port : int;
}

type t = {
  raft_configuration : Raft_types.configuration;
  servers_ipc_configuration : server_ipc_configuration list;
  storage_directory : string;
  app_server_port : int list;
  client_rate_limit : int; 
}

let default_configuration env =  
  
  let servers_ipc_configuration = 
    match env with 
    | `Gcp -> [
      {raft_id = 0; inet4_address = "10.142.0.3"; raft_port = 3500; client_port = 3501};
      {raft_id = 1; inet4_address = "10.142.0.4"; raft_port = 3500; client_port = 3501};
      {raft_id = 2; inet4_address = "10.142.0.5"; raft_port = 3500; client_port = 3501};
    ] 
    | `Mac -> [
      {raft_id = 0; inet4_address = "127.0.0.1"; raft_port = 34765; client_port = 34865};
      {raft_id = 1; inet4_address = "127.0.0.1"; raft_port = 34766; client_port = 34866};
      {raft_id = 2; inet4_address = "127.0.0.1"; raft_port = 34767; client_port = 34867};
    ]
  in

  {
    raft_configuration = RTypes.({
      nb_of_server = 3;
      election_timeout = 0.70;
      election_timeout_range = 0.2;
      hearbeat_timeout = 0.1;
      max_nb_logs_per_message = Raft_log.Bytes (10000, 8);
      max_log_size = {
        Raft_log.upper_bound = 500_000; 
        Raft_log.lower_bound = 400_000; 
      };
    });
  
    servers_ipc_configuration;
  
    storage_directory = "/tmp/";
  
    app_server_port = [40_000; 40_001; 40_002];

    client_rate_limit = 10000;
  }

let env_arg = 
  let env = ref `Mac in 
  let env_spec = Arg.Symbol (["Mac"; "Gcp"], (function
    | "Gcp" -> env := `Gcp
    | "Mac" -> env := `Mac
    | _ -> assert(false)
  ) ) in  
  (env, env_spec) 

let string_of_env = function
  | `Gcp -> "Gcp"
  | `Mac -> "Mac"

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
