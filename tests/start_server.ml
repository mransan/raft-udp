
module Conf = Raft_com_conf

let () = 
  let id   = ref (-1) in
  let id_spec = Arg.Set_int id in
  
  let log = ref false in 
  let log_spec = Arg.Set log  in

  let env, env_spec = Conf.env_arg in
  
  let task = ref "" in 

  Arg.parse [
    ("--env", env_spec , " : which env");
    ("--id" , id_spec  , " : server raft id");
    ("--log", log_spec , " : enable logging");
  ] (function
    | "counter" -> task := "counter_srv.native" 
    | "hash" -> task := "hash_srv.native" 
    | _ -> print_endline "Invalid app name"; exit 1
  ) "start_server [options] <app name>";
  
  if !task = ""
  then begin 
    print_endline "Missing app name"; 
    exit 1
  end;

  let {
    Conf.disk_backup = {Conf.log_record_directory};
    _ 
  } = Conf.default_configuration !env in 

  begin 
    ignore @@ 
      Sys.command @@ Printf.sprintf "rm -rf %s/*.data" log_record_directory; 
    ignore @@ 
      Sys.command @@ "rm -f *.log"
  end; 

  (* Launching App server *)

  begin 
    let args = !task :: [] in 
    let args = 
      if !log
      then args @ ("--log")::[]
      else args
    in 
    let args = args @ ("--id")::(string_of_int !id)::[] in
    let args = args @ ("--env")::(Conf.string_of_env !env) :: [] in 

    match Unix.fork () with
    | 0 -> Unix.execv !task (Array.of_list args)
    | _ -> ()
  end;
  
  (* Launching RAFT server *)

  let launch_raft_server = 

    let args = "server.native" :: [] in 
    let args = 
      if !log
      then args @ ("--log")::[]
      else args
    in 
    let args = args @ ("--id")::(string_of_int !id)::[] in
    let args = args @ ("--env")::(Conf.string_of_env !env) :: [] in 

    fun () -> 

      match Unix.fork () with
      | 0  -> Unix.execv "server.native" (Array.of_list args)
      | id -> id 
  in

  let rec loop () =
    let child_pid = launch_raft_server () in
    let timeout = Random.int 60 + 10 in 
    Unix.sleep timeout; 
    Printf.printf "Killing RAFT Server\n%!";
    Unix.kill child_pid Sys.sigkill;
    Unix.sleep 2;
    loop ()
  in 

  loop ()

