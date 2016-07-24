open Lwt.Infix

module Pb = Asset_pb

module Types = struct 
  type asset_state =
    | Owned       of Raft_cry.Pub.t 
    | In_transfer of Raft_cry.Pub.t 
    [@@deriving show]
  
  type asset = {
    url : string; 
    id  : string; 
    prev_tx_id : string;
    state : asset_state; 
  }
  [@@deriving show]
  
  module StringMap = struct 
    include Map.Make(struct 
      type t = string 
      let compare (x:string) (y:string) = Pervasives.compare x y
    end) 
  
    let pp f fmt t = 
      Format.fprintf fmt "[";
      iter (fun key v ->
        Format.fprintf fmt "{key: %s, value: %a}, " key f v
      ) t;  
      Format.fprintf fmt "[";
  end 
  
  type t = asset StringMap.t 
  [@@deriving show]

end [@@ocaml.warning "-32"] 
(* this is for the unused generated function show ... *)

include Types 
(* It looks like one has to do this in order to reuse 
 * the types in Types in a local module when calling Asset_utl.Make_validation
 *)

(* Asset related functions *)

let owner {state; _ } = 
  match state with
  | Owned owner_pub_key -> Some owner_pub_key 
  | In_transfer _ -> None 

let receiver {state; _ } = 
  match state with
  | In_transfer receiver_pub_key -> Some receiver_pub_key 
  | Owned _ -> None 

let prev_tx_id {prev_tx_id; _} = prev_tx_id 

(* Application related functions *)

let make () = StringMap.empty 

let find t asset_id = 
  match StringMap.find asset_id t with
  | asset -> Some asset 
  | exception Not_found -> None 

let add t asset_id asset = 
  StringMap.add asset_id asset t 

let replace_asset t asset_id f = 
  let asset = StringMap.find asset_id t in  
  StringMap.add asset_id (f asset) t 

let content_of_url url = 
  Lwt.return @@ "This is a dummy content of course" ^ url

module Validation = Asset_utl.Make_validation(struct 
  type asset = Types.asset
  let owner = owner 
  let receiver = receiver 
  let prev_tx_id = prev_tx_id

  type t = Types.t 
  let find = find  
end) 

let handle_tx t = function 
  | Pb.Issue_asset issue_asset ->
    let {Pb.a_url; a_hash} = issue_asset.Pb.ia_asset in
    content_of_url a_url 
    >|= (function url_content ->
      match Validation.validate_issue_asset issue_asset ~url_content t with
      | Validation.Ok {Validation.tx_id; ok_data = owner} ->
        let asset = { 
          url = a_url; 
          id  = a_hash;
          prev_tx_id = tx_id; 
          state = Owned owner;
        } in 
        add t a_hash asset 
      | Validation.Error -> t 
    )
  
  | Pb.Transfer transfer ->
    Lwt.return (match Validation.validate_transfer transfer t with
      | Validation.Ok {Validation.tx_id; ok_data = {Validation.tr_asset = _ ; tr_receiver}} -> 
        let {Pb.tr_asset_id; _ } = transfer in  
        replace_asset t tr_asset_id (fun asset -> 
          {asset with 
            prev_tx_id = tx_id; 
            state = In_transfer tr_receiver; 
          }  
        )
      | Validation.Error -> t
    ) 

  | Pb.Accept_transfer accept_transfer ->
    Lwt.return (match Validation.validate_accept_transfer accept_transfer t with
      | Validation.Ok {Validation.tx_id; ok_data = {Validation.at_asset = _ ; at_owner}} -> 
        let {Pb.at_asset_id; _ } = accept_transfer in  
        replace_asset t at_asset_id (fun asset -> 
            {asset with 
              prev_tx_id = tx_id; 
              state = Owned at_owner; 
            }  
        )
      | Validation.Error -> t
    ) 
