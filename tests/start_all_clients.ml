let arg_of_server i = 
  let arg = [| 
    "./client.native";
  |] in 
  arg

module Conf = Raft_udp_conf
module Udp  = Raft_udp_pb
   

let () = 

  for i = 1 to 400 do
    match Unix.fork () with
    | 0 -> Unix.execv "./client.native" (arg_of_server i)
    | _ -> ()
  done;

  ignore @@ Unix.wait ()
