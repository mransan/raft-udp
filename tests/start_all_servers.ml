let arg_of_server log i = 
  let arg = [| 
    "./server.native";
    "--id";
    string_of_int i;
    "--log";
    "";
  |] in 
  begin 
    if i = 1 
    then arg.(Array.length arg - 1) <- "--print-header" 
    else ();
  end;
  begin 
    if not log 
    then arg.(3) <- ""; 
  end;
  arg

module Conf = Raft_udp_conf
module Udp  = Raft_udp_pb
   
let () = 
  
  let task = ref "" in 
  let log = ref false in 
  let log_spec = Arg.Set log  in
  
  Arg.parse [
    ("--log", log_spec, " : enable logging");
  ] (function
    | "counter" -> task := "counter_srv.native" 
    | _ -> failwith "Invalid app name"
  ) "start_all_servers.native [options]";

  assert(!task <> "");

  let {Udp.servers_ipc_configuration ;_  } = Conf.default_configuration () in 

  let nb_of_servers = List.length servers_ipc_configuration in 

  begin 
    let args = [| !task; "" |] in 
    begin 
      if !log 
      then args.(1) <- "--log"
    end;
    match Unix.fork () with
    | 0 -> Unix.execv !task args
    | _ -> Unix.sleep 2;
  end;

  let rec aux acc = function
    | 0 -> acc 
    | i -> 
      let i = i - 1 in 
      match Unix.fork () with
      | 0   -> Unix.execv "./server.native" (arg_of_server !log i)
      | pid -> aux ((i, pid)::acc) i  
  in

  let processes = aux [] nb_of_servers in 

  let rec aux server_to_kill processes = 
    Unix.sleep 10000; 
    let server_to_kill = (server_to_kill + 1) mod nb_of_servers in 
    let processes = List.map (fun (server_id, pid) -> 
      if server_to_kill  = server_id
      then begin 
        Printf.printf "Killing server id: %i, PID: %i\n%!" server_id pid; 
        Unix.kill pid Sys.sigkill;
        Unix.sleep 1;  
        match Unix.fork () with
        | 0   -> Unix.execv "./server.native" (arg_of_server !log server_id)
        | pid ->
          begin  
            Printf.printf "Restarted server id: %i, PID: %i\n%!" server_id pid; 
            (server_id, pid)
          end
      end 
      else 
        (server_id, pid)
    ) processes in 
    aux server_to_kill processes
  in 
  aux 0 processes   
