module Pb = Asset_pb
module Cry= Raft_cry

let id_of_asset {Pb.url; hash} = 
  Cry.Sha256.hash_strings [url; Bytes.to_string hash]

let id_of_transfer {Pb.asset; dest_addr;} = 
  Cry.Sha256.hash_strings [id_of_asset asset; Bytes.to_string dest_addr]

let id_of_accept_transfer {Pb.transfer_id; } = 
  Cry.Sha256.hash_strings [Bytes.to_string transfer_id] 

let id_of_tx = function
  | Pb.Transfer x -> id_of_transfer x
  | Pb.Accept_transfer x -> id_of_accept_transfer x  

let signed_tx_of_tx tx prv_key = 
  let signature = 
    id_of_tx tx 
    |> Cry.Prv.sign prv_key 
    |> Cry.Sig.serialize 
  in
  {Pb.tx; signature}

let verify_signed_tx {Pb.tx; signature;} pub_key = 
  let id = id_of_tx tx in 
  Cry.Pub.verify pub_key id (Cry.Sig.deserialize signature) 
