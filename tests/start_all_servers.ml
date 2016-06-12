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
  
  let log = ref false in 
  let log_spec = Arg.Set log  in
  
  Arg.parse [
    ("--log", log_spec, " : enable logging");
  ] (fun _ -> ()) "start_all_server [options]";

  let {Udp.servers_udp_configuration ;_  } = Conf.default_configuration () in 

  let nb_of_servers = List.length servers_udp_configuration in 

  begin 
    let args = [| "./app.native" ; "" |] in 
    begin 
      if !log 
      then args.(1) <- "--log"
    end;
    match Unix.fork () with
    | 0 -> Unix.execv "./app.native" args
    | _ -> Unix.sleep 1;
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

  let rec aux processes = 
    Unix.sleep 20; 
    let server_to_kill = Random.int 3 in 
    let processes = List.map (fun (server_id, pid) -> 
      if server_to_kill  = server_id
      then begin 
        Printf.printf "Killing server id: %i, PID: %i\n%!" server_id pid; 
        Unix.kill pid Sys.sigkill;
        Unix.sleep 8;  
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
    aux processes
  in 
  aux processes   
