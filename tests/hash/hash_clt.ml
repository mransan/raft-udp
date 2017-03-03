open Lwt.Infix 

module Conf = Raft_com_conf

module Clt = Raft_app_clt.Make(struct

  type data = Hash_pb.app_data

  let encode data = 
    let encoder = Pbrt.Encoder.create () in 
    Hash_pb.encode_app_data data encoder; 
    Pbrt.Encoder.to_bytes encoder

  type result = Hash_pb.app_result 

  let decode bytes = 
    let decoder = Pbrt.Decoder.of_bytes bytes in 
    Hash_pb.decode_app_result decoder 

end)

let data_size = 16

let random_string () =
  let b = Bytes.create data_size in 
  for i = 0 to (data_size - 1)  do
    Bytes.set b i (char_of_int @@ 97 + Random.int 26)
  done; 
  Bytes.to_string b

let rec loop client () = 
  Clt.send client Hash_pb.({ random_data = random_string (); }) 
  >|=(function
    | None -> assert(false) 
    | Some _ -> ()
  )  
  >>= loop client 

let main () = 
  Lwt_log_core.default := Lwt_log_core.null; 
  Random.self_init (); 

  let env, env_spec = Conf.env_arg in 

  Arg.parse [
    ("--env", env_spec, " : which env"); 
  ] (fun _ -> ()) "hash_clt";

  Raft_app_clt.make (Raft_com_conf.default_configuration !env)
  >>=(fun client -> loop client ())

let () = ignore @@ Lwt_main.run (main ())
