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

module Conf = Raft_com_conf
   
let () = 
  
  let task = ref "" in 
  let log = ref false in 
  let log_spec = Arg.Set log  in
  
  Arg.parse [
    ("--log", log_spec, " : enable logging");
  ] (function
    | "counter" -> task := "counter_srv.native" 
    | "hash" -> task := "hash_srv.native" 
    | _ -> failwith "Invalid app name"
  ) "start_all_servers.native [options]";

  assert(!task <> "");

  let {
    Conf.servers_ipc_configuration; 
    Conf.storage_directory;
    _ 
  } = Conf.default_configuration `Mac in 

  begin 
    ignore @@ 
      Sys.command @@ Printf.sprintf "rm -rf %s/*.data" storage_directory; 
    ignore @@ 
      Sys.command @@ "rm -f *.log"
  end; 

  let nb_of_servers = List.length servers_ipc_configuration in 

  for i = 0 to nb_of_servers - 1 do 
    let args = [| !task; "--id"; (string_of_int i); "" |] in 
    begin 
      if !log 
      then args.(3) <- "--log"
    end;
    match Unix.fork () with
    | 0 -> Unix.execv !task args
    | _ -> ()
  done; 

  print_endline "All app servers launched...";

  Unix.sleep 2;

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
        Unix.sleep 10;  
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
