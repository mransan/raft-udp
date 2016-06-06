let arg_of_server i = 
  let arg = [| 
    "./server.native";
    "--id";
    string_of_int i;
    (*"--log"; *)
    "";
  |] in 
  begin 
    if i = 1 
    then arg.(Array.length arg - 1) <- "--print-header" 
    else ();
  end;
  arg

module Conf = Raft_udp_conf
module Udp  = Raft_udp_pb
   
let () = 

  let {Udp.servers_udp_configuration ;_  } = Conf.default_configuration () in 

  let nb_of_servers = List.length servers_udp_configuration in 

  let rec aux acc = function
    | 0 -> acc 
    | i -> 
      let i = i - 1 in 
      match Unix.fork () with
      | 0   -> Unix.execv "./server.native" (arg_of_server i)
      | pid -> aux ((i, pid)::acc) i  
  in

  let processes = aux [] nb_of_servers in 

  let rec aux processes = 
    Unix.sleep 30; 
    let server_to_kill = Random.int 3 in 
    let processes = List.map (fun (server_id, pid) -> 
      if server_to_kill  = server_id
      then begin 
        Printf.printf "Killing server id: %i, PID: %i\n%!" server_id pid; 
        Unix.kill pid Sys.sigkill;
        Unix.sleep 8;  
        match Unix.fork () with
        | 0   -> Unix.execv "./server.native" (arg_of_server server_id)
        | pid ->
          begin  
            Printf.printf "Restarted server id: %i, PID: %i\n%!" server_id pid; 
            (server_id, pid)
          end
      end 
      else 
        (server_id, pid)
    ) processes in 
    aux processes
  in 
  aux processes   
