module Pb = Asset_pb
module Cry = Raft_cry
  
type tx_id = string 

module type App_sig = sig 

  type asset 

  val owner : asset -> Cry.Pub.t option 

  val receiver : asset -> Cry.Pub.t option 
  
  val prev_tx_id : asset -> string 

  type t

  val find : t -> string -> asset option 

end 

let id_of_issue_asset ~ia_asset_id ~ia_issuer_addr () = 
  Cry.Sha256.hash_strings [ia_asset_id; ia_issuer_addr] 

let id_of_transfer ~prev_tx_id ~tr_asset_id ~tr_dest_addr () =
  Cry.Sha256.hash_strings [prev_tx_id; tr_asset_id; tr_dest_addr] 

let id_of_accept_transfer ~prev_tx_id ~at_asset_id () = 
  Cry.Sha256.hash_strings [prev_tx_id; at_asset_id] 

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

let sign_id ~id ~prv_key () = 
  id
  |> Cry.Prv.sign prv_key
  |> Cry.Sig.to_binary 
  |> B58.encode_str 

let verify_id ~id ~sig_ ~pub_key () = 
  let sig_ = sig_ |> B58.decode_str |> Cry.Sig.from_binary in 
  Cry.Pub.verify pub_key id sig_

let pub_key_of_addr addr = 
  addr
  |> B58.decode_str 
  |> Cry.Pub.from_binary 

let addr_of_pub_key pub_key = 
  pub_key |> Cry.Pub.to_binary |> B58.encode_str 

let make_asset ~url ~url_content () = 
  let a_id = 
    Cry.Sha256.hash_strings [url_content]
    |> B58.encode_str
  in 
  {Pb.a_id; a_url = url}

let make_issue_asset ~url ~url_content ~prv_key () = 
  let ia_asset = make_asset ~url ~url_content () in 
  let ia_issuer_addr = 
    Cry.Prv.public_key prv_key 
    |> Cry.Pub.to_binary 
    |> B58.encode_str
  in  

  let id = id_of_issue_asset ~ia_asset_id:ia_asset.Pb.a_id ~ia_issuer_addr () in 
  let ia_sig = sign_id ~id ~prv_key () in  
  ({Pb.ia_asset; ia_issuer_addr; ia_sig}, id) 

type dest_addr = 
  | Binary of Cry.Pub.t 
  | Text   of string 

let make_transfer ~prev_tx_id ~asset_id ~dest_addr ~prv_key () = 
  let tr_asset_id = asset_id in 
  let tr_dest_addr = match dest_addr with
    | Binary k -> k |> Cry.Pub.to_binary |> B58.encode_str 
    | Text   k -> k 
  in 
  let id = id_of_transfer ~prev_tx_id ~tr_asset_id ~tr_dest_addr () in 
  let tr_sig = sign_id ~id ~prv_key () in  
  ({Pb.tr_asset_id = asset_id; tr_dest_addr; tr_sig}, id)

let make_accept_transfer ~prev_tx_id ~asset_id ~prv_key () = 
  let at_asset_id = asset_id in 
  let id = id_of_accept_transfer ~prev_tx_id ~at_asset_id () in
  let at_sig = sign_id ~id ~prv_key () in  
  ({Pb.at_asset_id; at_sig}, id) 

   
module Make_validation(App:App_sig) = struct 

  type 'a ok_result = {
    tx_id : tx_id; 
    ok_data : 'a; 
  }
  
  type validation_error =
    | Invalid_asset_id
    | Duplicate_asset of string 
    | Invalid_signature of string * string * string * (string option)  
    | Unknown_asset of string 
    | Attempt_to_transfer_in_transfer_asset of string  
    | Asset_not_in_transfer of string 

  let string_of_validation_error = function
    | Invalid_asset_id -> "Invalid asset id" 

    | Duplicate_asset asset_id -> 
      Printf.sprintf "Duplicate asset, id: %s" asset_id 

    | Invalid_signature  (id, sig_, pub_key, error_msg) -> 
      let error_msg = match error_msg with
        | None -> "No detail error msg"
        | Some error_msg -> error_msg
      in 
      Printf.sprintf 
        "Invalid transaction signature, id in base58: %s, sig in base58: %s, public key in base58: %s, error_msg: %s" 
        id sig_ pub_key error_msg

    | Unknown_asset asset_id -> 
      Printf.sprintf "Unknown asset, id: %s" asset_id

    | Attempt_to_transfer_in_transfer_asset asset_id -> 
      Printf.sprintf "Attempt to transfer an 'in transfer' asset, id: %s" asset_id

    | Asset_not_in_transfer asset_id -> 
      Printf.sprintf "Asset is not in transfer, id: %s" asset_id 

  type 'a validation_result = 
    | Validation_ok of 'a ok_result  
    | Validation_error of validation_error  

  let make_invalid_signature ?error_msg ~id ~sig_ ~pub_key () = 
    Validation_error (
      Invalid_signature (B58.encode_str id, sig_, addr_of_pub_key pub_key, error_msg)
    ) 
     
  let verify_id ~id ~sig_ ~pub_key f = 
    try
      if verify_id ~id ~sig_ ~pub_key () 
      then Validation_ok {tx_id = id; ok_data = (f ())} 
      else make_invalid_signature ~id ~sig_ ~pub_key () 
    with 
    | Cry.Cry_failure error_msg -> 
      make_invalid_signature ~error_msg ~id ~sig_ ~pub_key ()

  type issue_asset_ok = Cry.Pub.t  
  
  let validate_asset {Pb.a_id; _} ~url_content app = 
    (* 
     * TODO
     * Also check that [a_url] is not already in used by a previously issued
     * asset.
     * If the same url is used with different content, the validation below
     * will succeed. However this is not correct since replaying the entire
     * transaction since inception will then fail. 
     *)
    let a_id' = Cry.Sha256.hash_strings [url_content] |> B58.encode_str in 
    if a_id' <> a_id
    then Error Invalid_asset_id
    else 
      match App.find app a_id with
      | None -> Ok ()
      | Some _ -> Error (Duplicate_asset a_id)

  let validate_issue_asset issue_asset ~url_content app = 
    let {Pb.ia_asset; ia_issuer_addr; ia_sig} = issue_asset in 
    match validate_asset ia_asset ~url_content app with
    | Error e -> Validation_error e 
    | Ok () ->  
      let pub_key = pub_key_of_addr ia_issuer_addr in  
      let id = id_of_issue_asset ~ia_asset_id:ia_asset.Pb.a_id ~ia_issuer_addr () in  
      verify_id ~id ~pub_key ~sig_:ia_sig (fun () ->
        pub_key
      ) 

  type transfer_ok = {
    tr_asset : App.asset; 
    tr_receiver : Cry.Pub.t; 
  }

  let validate_transfer transfer app = 
    let {Pb.tr_asset_id; tr_sig; tr_dest_addr} = transfer in 
    match App.find app tr_asset_id with
    | None -> Validation_error (Unknown_asset tr_asset_id) 
    | Some asset -> 
      match App.owner asset with
      | None -> 
        Validation_error (Attempt_to_transfer_in_transfer_asset tr_asset_id) 
      | Some owner -> 
        let prev_tx_id = App.prev_tx_id asset in 
        let id = id_of_transfer ~prev_tx_id ~tr_asset_id ~tr_dest_addr () in 
        verify_id ~id ~pub_key:owner ~sig_:tr_sig (fun () -> 
          {
            tr_asset = asset; 
            tr_receiver = pub_key_of_addr tr_dest_addr; 
          }
        ) 

  type accept_transfer_ok = {
    at_asset : App.asset; 
    at_owner : Cry.Pub.t; 
  }

  let validate_accept_transfer accept_transfer app = 
    let {Pb.at_asset_id;at_sig} = accept_transfer in  
    match App.find app at_asset_id with
    | None -> Validation_error (Unknown_asset at_asset_id) 
    | Some asset -> 
      match App.receiver asset with
      | None -> 
        Validation_error (Asset_not_in_transfer at_asset_id) 
      | Some receiver -> 
        let prev_tx_id = App.prev_tx_id asset in 
        let id = id_of_accept_transfer ~prev_tx_id ~at_asset_id () in 
        verify_id ~id ~pub_key:receiver ~sig_:at_sig (fun () ->
          {
            at_asset = asset; 
            at_owner = receiver;
          }
        )  
end 
