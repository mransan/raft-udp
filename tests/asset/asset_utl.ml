module Pb = Asset_pb
module Cry= Raft_cry

module type App_sig = sig 

  type asset 

  val owner : asset -> string option 

  val receiver : asset -> string option 

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

module Make(App:App_sig) = struct 

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
    if not @@ validate_asset ia_asset url_content app 
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

end 
