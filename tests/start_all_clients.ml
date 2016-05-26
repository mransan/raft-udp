let arg_of_server i = 
  let arg = [| 
    "./client.native";
  |] in 
  arg

module Conf = Raft_udp_conf
module Udp  = Raft_udp_pb
   

let () = 

  let nb_of_childrend = 100 in 

  for i = 1 to nb_of_childrend do
    match Unix.fork () with
    | 0 -> Unix.execv "./client.native" (arg_of_server i)
    | _ -> ()
  done;
  
  for i = 0 to nb_of_childrend do
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
