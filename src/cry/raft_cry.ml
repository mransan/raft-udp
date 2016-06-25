module Hex = struct 

  let encode = 
    let transform = Cryptokit.Hexa.encode ()in 
    Cryptokit.transform_string transform 
  
  let decode = 
    let transform = Cryptokit.Hexa.decode ()in 
    Cryptokit.transform_string transform

end 

let size          = 256 (* Bits *) 
let size_in_bytes = 256 / 8 

module Sig = struct 

  type t = string 

  let serialize t = Hex.encode t  

  let deserialize s = Hex.decode s

end 

module Pub = struct 

  type t = {
    n : string; 
    e : string
  } 

  let serialized_size = 2 * size_in_bytes 

  let serialize {n; e} = 
    assert(String.length n = size_in_bytes);
    assert(String.length e = size_in_bytes);
    Hex.encode (n ^ e)
  
  let deserialize s = 
    assert(String.length s = serialized_size * 2);
    let s = Hex.decode s in 
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

  let make () = 
    Cryptokit.RSA.new_key size 

  let public_key {Cryptokit.RSA.n; e; _} = 
    Pub.({n; e})

  let sign key msg = 
    let hash     = Cryptokit.Hash.sha256 () in
    let msg_hash = String.sub (Cryptokit.hash_string hash msg) 0 31 in 
    Cryptokit.RSA.sign key msg_hash

  let serialize {Cryptokit.RSA.n;e;d;p;q;dp;dq;qinv;_} = 
    assert(String.length n = size_in_bytes);
    assert(String.length e = size_in_bytes);
    assert(String.length d = size_in_bytes);
    assert(String.length p = size_in_bytes/2);
    assert(String.length q = size_in_bytes/2);
    assert(String.length dp = size_in_bytes/2);
    assert(String.length dq = size_in_bytes/2);
    assert(String.length qinv = size_in_bytes/2);
    let s = String.concat "" [n;e;d;p;q;dp;dq;qinv] in 
    Hex.encode s 

  let deserialize s = 
    let half = size_in_bytes/2 in 
    assert(String.length s = (size_in_bytes * 5 + half) * 2);
    let s = Hex.decode s in 
    Cryptokit.RSA.({
      size = size;
      n    = String.sub s (0                       ) size_in_bytes; 
      e    = String.sub s (size_in_bytes           ) size_in_bytes; 
      d    = String.sub s (size_in_bytes * 2       ) size_in_bytes; 
      p    = String.sub s (size_in_bytes * 3       ) half; 
      q    = String.sub s (size_in_bytes * 3 + half) half;
      dp   = String.sub s (size_in_bytes * 4       ) half;
      dq   = String.sub s (size_in_bytes * 4 + half) half;
      qinv = String.sub s (size_in_bytes * 5       ) half;
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
