
module Pb  = Raft_udp_pb
module RPb = Raft_pb 

let default_configuration () = Pb.({
  raft_configuration = RPb.({
    nb_of_server = 3;
    election_timeout = 2.;
    election_timeout_range = 0.5;
    hearbeat_timeout = 0.2;
    max_nb_logs_per_message = 300;
    log_interval_size = 10_000;
  });
  servers_udp_configuration = [
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
  compaction_period = 1.;
  log_record_directory = "/tmp/";
  compaction_directory = "/tmp/";
})

let sockaddr_of_server_config which {Pb.inet4_address; raft_port; client_port} =
  let port = match which with
    | `Client  -> client_port 
    | `Raft    -> raft_port
  in 
  Unix.ADDR_INET (Unix.inet_addr_of_string inet4_address, port)

let sockaddr_of_server_id which configuration server_id =
  let is_server {Pb.raft_id; _ } =
    raft_id = server_id
  in
  match List.find is_server configuration.Pb.servers_udp_configuration with
  | server_config -> Some (sockaddr_of_server_config which server_config)
  | exception Not_found -> None
