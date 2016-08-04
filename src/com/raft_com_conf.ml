
module Com_pb = Raft_com_pb
module RPb = Raft_pb 

let default_configuration () = Com_pb.({
  raft_configuration = RPb.({
    nb_of_server = 3;
    election_timeout = 0.50;
    election_timeout_range = 0.2;
    hearbeat_timeout = 0.1;
    max_nb_logs_per_message = 300;
    log_interval_size = 1_000;
  });

  servers_ipc_configuration = [
    {
      raft_id = 0; 
      inet4_address = "127.0.0.1"; 
      raft_port = 34765; 
      client_port = 34865;
      app_server_port = 40_000;
    };
    {
      raft_id = 1; 
      inet4_address = "127.0.0.1"; 
      raft_port = 34766; 
      client_port = 34866;
      app_server_port = 40_001;
    };
    {
      raft_id = 2; 
      inet4_address = "127.0.0.1"; 
      raft_port = 34767; 
      client_port = 34867;
      app_server_port = 40_002;
    };
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
    compaction_period = 1.;
    log_record_directory = "/tmp/";
    compaction_directory = "/tmp/";
  };
})

let sockaddr_of_server_config which {Com_pb.inet4_address; raft_port; client_port; _ } =
  let port = match which with
    | `Client  -> client_port 
    | `Raft    -> raft_port
  in 
  Unix.ADDR_INET (Unix.inet_addr_of_string inet4_address, port)

let sockaddr_of_server_id which configuration server_id =
  let is_server {Com_pb.raft_id; _ } =
    raft_id = server_id
  in
  match List.find is_server configuration.Com_pb.servers_ipc_configuration with
  | server_config -> Some (sockaddr_of_server_config which server_config)
  | exception Not_found -> None

let get_id_cmdline {Com_pb.servers_ipc_configuration; _ } = 
  let nb_of_servers = List.length servers_ipc_configuration in 

  let ids = 
    let rec aux acc = function
      | -1 -> acc
      | n  -> aux ((string_of_int n)::acc) (n - 1)
    in 
    aux [] (nb_of_servers - 1)
  in

  let id   = ref (-1) in
  let id_spec = Arg.Symbol (ids, fun s ->
    id := int_of_string s
  ) in

  (id, id_spec) 

let server_ipc_configuration  {Com_pb.servers_ipc_configuration; _ }  server_id = 

  if (server_id < List.length servers_ipc_configuration) && 
     (server_id >= 0)
  then Some (List.nth servers_ipc_configuration server_id) 
  else None 
