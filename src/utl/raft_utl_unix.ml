let string_of_sockaddr = function
  | Unix.ADDR_UNIX addr -> 
    Printf.sprintf "ADDR_UNIX(%s)" addr
  | Unix.ADDR_INET (addr, port) -> 
    Printf.sprintf "ADDR_INET(address: %s, port: %i)" (Unix.string_of_inet_addr addr) port
