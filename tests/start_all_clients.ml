let arg_of_server ~task ~log _ = 

  let arg = [| 
    task;
    "";
  |] in 
  begin if log then arg.(1) <- "--log"; end; 
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
    | "counter" -> task := "./counter_clt.native" 
    | "asset" -> task := "./asset_clt.native" 
    | _ -> failwith "Invalid app name"
  ) "start_all_clients.native";

  assert(!task <> "");

  let nb_of_children = 10 in 

  for i = 1 to nb_of_children do
    match Unix.fork () with
    | 0 -> Unix.execv !task (arg_of_server ~task:!task ~log:!log i)
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
