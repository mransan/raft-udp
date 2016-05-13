
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
    {raft_id = 0; inet4_address = "172.17.0.2"; port = 12000; };
    {raft_id = 1; inet4_address = "172.17.0.3"; port = 12000; };
    {raft_id = 2; inet4_address = "172.17.0.4"; port = 12000; };
  ];
})

let sockaddr_of_server_config {Udp.inet4_address; port} =
  Unix.ADDR_INET (Unix.inet_addr_of_string inet4_address, port)

let sockaddr_of_server_id configuration server_id =
  let is_server {Udp.raft_id; _ } =
    raft_id = server_id
  in
  match List.find is_server configuration.Udp.servers_udp_configuration with
  | server_config -> Some (sockaddr_of_server_config server_config)
  | exception Not_found -> None
