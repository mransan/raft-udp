
module Udp  = Raft_udp_pb
module Raft = Raft_pb 

let default_configuration () = Udp.({
  raft_configuration = Raft.({
    nb_of_server = 3;
    election_timeout = 1.;
    election_timeout_range = 0.1;
    hearbeat_timeout = 0.2;
    max_nb_logs_per_message = 300;
  });
  servers_udp_configuration = [
    {raft_id = 0; inet4_address = "127.0.0.1"; raft_port = 34765; client_port = 34865};
    {raft_id = 1; inet4_address = "127.0.0.1"; raft_port = 34766; client_port = 34866};
    {raft_id = 2; inet4_address = "127.0.0.1"; raft_port = 34767; client_port = 34867};
  ];
})

let sockaddr_of_server_config which {Udp.inet4_address; raft_port; client_port} =
  let port = match which with
    | `Client  -> client_port 
    | `Raft    -> raft_port
  in 
  Unix.ADDR_INET (Unix.inet_addr_of_string inet4_address, port)

let sockaddr_of_server_id which configuration server_id =
  let is_server {Udp.raft_id; _ } =
    raft_id = server_id
  in
  match List.find is_server configuration.Udp.servers_udp_configuration with
  | server_config -> Some (sockaddr_of_server_config which server_config)
  | exception Not_found -> None
