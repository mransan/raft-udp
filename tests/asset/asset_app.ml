open Lwt.Infix
open !Lwt_log_core

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

let handle_tx ~logger t = function 
  | Pb.Issue_asset issue_asset ->
    let {Pb.a_url; a_id} = issue_asset.Pb.ia_asset in
    content_of_url a_url 
    >>= (function url_content ->
      log_f 
        ~logger 
        ~level:Notice 
        "Validating issue asset: %s" (Asset_pb.show_issue_asset issue_asset)
      >|=(fun () ->
        match Validation.validate_issue_asset issue_asset ~url_content t with
        | Validation.Validation_ok {Validation.tx_id; ok_data = owner} ->
          let asset = { 
            url = a_url; 
            id  = a_id;
            prev_tx_id = tx_id; 
            state = Owned owner;
          } in 
          Ok (add t a_id asset) 
        | Validation.Validation_error e -> 
          Error (Validation.string_of_validation_error e) 
      )
    )
  
  | Pb.Transfer transfer ->
    Lwt.return (match Validation.validate_transfer transfer t with
      | Validation.Validation_ok ok_transfer -> 
        let {
          Validation.tx_id; 
          ok_data = {Validation.tr_asset = _ ; tr_receiver}
        } = ok_transfer in 
        let {Pb.tr_asset_id; _ } = transfer in  
        let t = replace_asset t tr_asset_id (fun asset -> 
          {asset with 
            prev_tx_id = tx_id; 
            state = In_transfer tr_receiver; 
          }  
        ) in
        Ok t 
      | Validation.Validation_error e -> 
        Error (Validation.string_of_validation_error e) 
    ) 

  | Pb.Accept_transfer accept_transfer ->
    Lwt.return (match Validation.validate_accept_transfer accept_transfer t with
      | Validation.Validation_ok ok_accept_transfer -> 
        let {
          Validation.tx_id; 
          ok_data = {Validation.at_asset = _ ; at_owner}
        } = ok_accept_transfer in 
        let {Pb.at_asset_id; _ } = accept_transfer in  
        let t = replace_asset t at_asset_id (fun asset -> 
            {asset with 
              prev_tx_id = tx_id; 
              state = Owned at_owner; 
            }  
        ) in 
        Ok t 
      | Validation.Validation_error e -> 
        Error (Validation.string_of_validation_error e) 
    ) 
