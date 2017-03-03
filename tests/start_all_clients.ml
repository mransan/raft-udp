module Conf = Raft_com_conf

let arg_of_server log env task _ = 

  let arg = [| 
    task;
    "--log";
    "--env"; 
    (Conf.string_of_env env);
  |] in 
  if not log 
  then arg.(1) <- "";
  arg

   
let () = 

  let task = ref "" in 

  let log = ref false in 
  let log_spec = Arg.Set log in 

  let env, env_spec = Conf.env_arg in 

  Arg.parse [
    ("--env", env_spec, " : which env");
    ("--log", log_spec, " : enable logging");
  ] (function 
    | "counter" -> task := "./counter_clt.native" 
    | "hash" -> task := "./hash_clt.native" 
    | _ -> failwith "Invalid app name"
  ) "start_all_clients.native";

  assert(!task <> "");

  let nb_of_children = 100 in 

  for i = 1 to nb_of_children do
    match Unix.fork () with
    | 0 -> Unix.execv !task (arg_of_server !log !env !task i)
    | _ -> ()
  done;
  
  for _ = 0 to nb_of_children do
    let pid, process_status = Unix.wait () in 
    Printf.eprintf "Process [%5i] died with status %s\n%!"
      pid
      ((function 
        | Unix.WEXITED i   -> Printf.sprintf "WEXITED(%i)" i
        | Unix.WSIGNALED i -> Printf.sprintf "WSIGNALED(%i)" i 
        | Unix.WSTOPPED i  -> Printf.sprintf "WSTOPPED(%i)" i
      ) process_status) 
  done;

  ignore @@ Unix.wait ()
