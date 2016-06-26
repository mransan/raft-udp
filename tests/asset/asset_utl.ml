module Pb = Asset_pb
module Cry= Raft_cry

module type App_sig = sig 

  type asset 

  val owner : asset -> Cry.Pub.t option 

  val receiver : asset -> Cry.Pub.t option 

  type t

  val find : t -> string -> asset option 

end 

let id_of_asset {Pb.a_url = _ ; a_hash} = 
  a_hash

let id_of_issue_asset {Pb.ia_asset; ia_issuer_addr; ia_sig = _ } = 
  Cry.Sha256.hash_strings [id_of_asset ia_asset; ia_issuer_addr] 

let id_of_transfer {Pb.tr_asset_id; tr_dest_addr; tr_sig = _} = 
  Cry.Sha256.hash_strings [tr_asset_id; tr_dest_addr]

let id_of_accept_transfer {Pb.at_asset_id; at_sig = _ } = 
  at_asset_id

module B58 = struct
  include B58 
  
  let alphabet = B58.make_alphabet "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

  let encode_str s = 
    Bytes.of_string s 
    |> encode alphabet 
    |> Bytes.to_string 

  let decode_str s = 
    Bytes.of_string s 
    |> decode alphabet 
    |> Bytes.to_string 
end 

let make_asset ~url ~url_content () = 
  let a_hash = 
    Cry.Sha256.hash_strings [url_content]
    |> B58.encode_str
  in 
  {Pb.a_hash; a_url = url}

let make_issue_asset ~url ~url_content ~prv_key () = 
  let ia_asset = make_asset ~url ~url_content () in 

  let ia_issuer_addr = 
    Cry.Prv.public_key prv_key 
    |> Cry.Pub.to_binary 
    |> B58.encode_str
  in  

  let id = Cry.Sha256.hash_strings [id_of_asset ia_asset; ia_issuer_addr] in 
  let ia_sig = 
    Cry.Prv.sign prv_key id  
    |> Cry.Sig.to_binary 
    |> B58.encode_str 
  in 
  {Pb.ia_asset; ia_issuer_addr; ia_sig} 

type dest_addr = 
  | Binary of Cry.Pub.t 
  | Text   of string 

let make_transfer ~asset_id ~dest_addr ~prv_key () = 
  let tr_dest_addr = match dest_addr with
    | Binary k -> k |> Cry.Pub.to_binary |> B58.encode_str 
    | Text   k -> k 
  in 
  let id  = Cry.Sha256.hash_strings [asset_id; tr_dest_addr] in 
  let tr_sig = 
    id 
    |> Cry.Prv.sign prv_key  
    |> Cry.Sig.to_binary 
    |> B58.encode_str 
  in 
  {Pb.tr_asset_id = asset_id; tr_dest_addr; tr_sig}
   
module Make_validation(App:App_sig) = struct 

  let validate_asset {Pb.a_hash; _} ~url_content app = 
    let a_hash' = 
      Cry.Sha256.hash_strings [url_content]
      |> B58.encode_str 
    in 
    if a_hash <> a_hash'
    then false 
    else 
      match App.find app a_hash with
      | None -> true
      | Some _ -> false 

  let validate_issue_asset issue_asset ~url_content app = 
    let {Pb.ia_asset; ia_issuer_addr; ia_sig} = issue_asset in 
    if not @@ validate_asset ia_asset ~url_content app 
    then 
      false 
    else 
      let pub_key = 
        ia_issuer_addr
        |> B58.decode_str 
        |> Cry.Pub.from_binary 
      in 
      let sig_ = 
        ia_sig
        |> B58.decode_str 
        |> Cry.Sig.from_binary 
      in
      Cry.Pub.verify pub_key (id_of_issue_asset issue_asset) sig_ 

  let validate_transfer transfer app = 
    let {Pb.tr_asset_id; tr_sig; _ } = transfer in 
    match App.find app tr_asset_id with
    | None -> false 
    | Some asset -> 
      match App.owner asset with
      | None -> false 
      | Some owner -> 
        let sig_ = tr_sig |> B58.decode_str |> Cry.Sig.from_binary in 
        Cry.Pub.verify owner (id_of_transfer transfer) sig_
         
end 
