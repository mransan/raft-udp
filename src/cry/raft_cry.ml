module Hex = struct 
  let encode = 
    let transform = Cryptokit.Hexa.encode ()in 
    Cryptokit.transform_string transform 
end 

let size          = 256 (* Bits *) 
let size_in_bytes = 256 / 8 

module Sig = struct 

  type t = string 

  let to_binary t = t 

  let from_binary s = s

end 

module Pub = struct 

  type t = {
    n : string; 
    e : string
  } 

  let to_binary {n; e} = 
    assert(String.length n = size_in_bytes);
    assert(String.length e = size_in_bytes);
    (n ^ e)
  
  let serialized_size = 2 * size_in_bytes 

  let from_binary s = 
    assert(String.length s = serialized_size);
    {
      n = String.sub s 0 size_in_bytes; 
      e = String.sub s size_in_bytes size_in_bytes;
    } 

  let verify {n;e} msg sign = 
    let ckey = Cryptokit.({
      RSA.size; n; e; d = ""; p = ""; q = ""; dp = ""; dq = ""; qinv = "";
    }) in 
    let hash     = Cryptokit.Hash.sha256 () in
    let msg_hash = String.sub (Cryptokit.hash_string hash msg) 0 31 in 
    let msg_hash'= String.sub (Cryptokit.RSA.unwrap_signature ckey sign) 1 31 in 
    msg_hash = msg_hash' 

  let pp fmt {n;e} = 
    Format.fprintf fmt "{n: %s; e: %s}" (Hex.encode n) (Hex.encode e) 
end 

module Prv = struct 

  type t = Cryptokit.RSA.key 

  let to_string which key = 
    let module RSA = Cryptokit.RSA in 
    match which with 
    | `Public -> 
      Printf.sprintf "{\n size: %i, \n n(%i): %s, \n e(%i): %s\n}" 
        key.RSA.size 
        (String.length key.RSA.n) 
        (Hex.encode key.RSA.n)
        (String.length key.RSA.e) 
        (Hex.encode key.RSA.e)
    | `Private ->
      Printf.sprintf "{\n size: %i, \n n(%i): %s, \n d(%i): %s\n}" 
        key.RSA.size 
        (String.length key.RSA.n) 
        (Hex.encode key.RSA.n)
        (String.length key.RSA.d) 
        (Hex.encode key.RSA.d)
    | `Additional_component ->
      Printf.sprintf "{\n p(%i): %s, \n q(%i): %s, \n dp(%i): %s, \n dq(%i): %s, \n qinv(%i): %s\n}"
        (String.length key.RSA.p) 
        (Hex.encode key.RSA.p)
        (String.length key.RSA.q) 
        (Hex.encode key.RSA.q)
        (String.length key.RSA.dp) 
        (Hex.encode key.RSA.dp)
        (String.length key.RSA.dq) 
        (Hex.encode key.RSA.dq)
        (String.length key.RSA.qinv) 
        (Hex.encode key.RSA.qinv)

  let _ = to_string 

  let make () = 
    Cryptokit.RSA.new_key size 

  let public_key {Cryptokit.RSA.n; e; _} = 
    Pub.({n; e})

  let sign key msg = 
    let hash     = Cryptokit.Hash.sha256 () in
    let msg_hash = String.sub (Cryptokit.hash_string hash msg) 0 31 in 
    Cryptokit.RSA.sign key msg_hash

  let half_size = size_in_bytes / 2 

  let to_binary {Cryptokit.RSA.n;e;d;p;q;dp;dq;qinv;_} = 
    assert(String.length n = size_in_bytes);
    assert(String.length e = size_in_bytes);
    assert(String.length d = size_in_bytes);
    assert(String.length p = half_size);
    assert(String.length q = half_size);
    assert(String.length dp = half_size);
    assert(String.length dq = half_size);
    assert(String.length qinv = half_size);
    String.concat "" [n;e;d;p;q;dp;dq;qinv]

  let serialized_size = size_in_bytes * 5 + half_size 

  let from_binary s = 
    assert(String.length s = serialized_size);
    Cryptokit.RSA.({
      size = size;
      n    = String.sub s (0                            ) size_in_bytes; 
      e    = String.sub s (size_in_bytes                ) size_in_bytes; 
      d    = String.sub s (size_in_bytes * 2            ) size_in_bytes; 
      p    = String.sub s (size_in_bytes * 3            ) half_size; 
      q    = String.sub s (size_in_bytes * 3 + half_size) half_size;
      dp   = String.sub s (size_in_bytes * 4            ) half_size;
      dq   = String.sub s (size_in_bytes * 4 + half_size) half_size;
      qinv = String.sub s (size_in_bytes * 5            ) half_size;
    })

end 

module Sha256 = struct

  let hash_strings l = 
    let sha256 = Cryptokit.Hash.sha256 () in   
    let rec aux = function
      | [] -> sha256#result 
      | hd::tl -> sha256#add_string hd; aux tl 
    in 
    aux l 

end 
