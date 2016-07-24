let arg_of_server task _ = 

  let arg = [| 
    task;
    "--log";
  |] in 
  arg

module Conf = Raft_udp_conf
module Udp  = Raft_udp_pb
   

let () = 

  let task = ref "" in 

  Arg.parse [
  ] (function 
    | "counter" -> task := "./counter_clt.native" 
    | "asset" -> task := "./asset_clt.native" 
    | _ -> failwith "Invalid app name"
  ) "start_all_clients.native";

  assert(!task <> "");

  let nb_of_children = 1 in 

  for i = 1 to nb_of_children do
    match Unix.fork () with
    | 0 -> Unix.execv !task (arg_of_server !task i)
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
