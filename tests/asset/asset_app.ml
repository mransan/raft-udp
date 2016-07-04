open Lwt.Infix

module Pb = Asset_pb

module App = struct 
  type asset_state =
    | Owned       of Raft_cry.Pub.t 
    | In_transfer of Raft_cry.Pub.t 
  
  type asset = {
    url : string; 
    id  : string; 
    prev_tx_id : string;
    state : asset_state; 
  }
  
  let owner {state; _ } = 
    match state with
    | Owned owner_pub_key -> Some owner_pub_key 
    | In_transfer _ -> None 
  
  let receiver {state; _ } = 
    match state with
    | In_transfer receiver_pub_key -> Some receiver_pub_key 
    | Owned _ -> None 
  
  let prev_tx_id {prev_tx_id; _} = prev_tx_id 
  
  
  module StringMap = Map.Make(struct 
    type t = string 
    let compare (x:string) (y:string) = Pervasives.compare x y
  end) 
  
  type t = asset StringMap.t 
  
  let find t asset_id = 
    match StringMap.find asset_id t with
    | asset -> Some asset 
    | exception Not_found -> None 

  let add t asset_id asset = 
    StringMap.add asset_id asset t 

  let replace_asset t asset_id f = 
    let asset = StringMap.find asset_id t in  
    StringMap.add asset_id (f asset) t 

end 

module Val = Asset_utl.Make_validation(App) 

let content_of_url url = 
  Lwt.return @@ "This is a dummy content of course" ^ url

let handle_tx t = function 
  | Pb.Issue_asset issue_asset ->
    let {Pb.a_url; a_hash} = issue_asset.Pb.ia_asset in
    content_of_url a_url 
    >|= (function url_content ->
      match Val.validate_issue_asset issue_asset ~url_content t with
      | Val.Ok {Val.tx_id; ok_data = owner} ->
        let asset = { 
          App.url = a_url; 
          id  = a_hash;
          prev_tx_id = tx_id; 
          state = App.Owned owner;
        } in 
        App.add t a_hash asset 
      | Val.Error -> t 
    )
  
  | Pb.Transfer transfer ->
    Lwt.return (match Val.validate_transfer transfer t with
      | Val.Ok {Val.tx_id; ok_data = {Val.tr_asset = _ ; tr_receiver}} -> 
        let {Pb.tr_asset_id; _ } = transfer in  
        App.replace_asset t tr_asset_id (fun asset -> 
          {asset with 
            App.prev_tx_id = tx_id; 
            App.state = App.In_transfer tr_receiver; 
          }  
        )
      | Val.Error -> t
    ) 

  | Pb.Accept_transfer accept_transfer ->
    Lwt.return (match Val.validate_accept_transfer accept_transfer t with
      | Val.Ok {Val.tx_id; ok_data = {Val.at_asset = _ ; at_owner}} -> 
        let {Pb.at_asset_id; _ } = accept_transfer in  
        App.replace_asset t at_asset_id (fun asset -> 
            {asset with 
              App.prev_tx_id = tx_id; 
              App.state = App.Owned at_owner; 
            }  
        )
      | Val.Error -> t
    ) 
