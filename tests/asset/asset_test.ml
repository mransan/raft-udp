module Pb = Asset_pb
module Cry= Raft_cry

module App = struct

  type asset = string 

  let owner_key = Cry.Prv.make () 

  let receiver_key = Cry.Prv.make () 

  let owner _ = 
    Some (Cry.Prv.public_key owner_key)

  let receiver _ = 
    Some (Cry.Prv.public_key receiver_key)

  let prev_tx_id_str = "Fake previous tx id"

  let prev_tx_id _ = prev_tx_id_str

  type t = asset list 

  let find t asset_id =
    if List.mem asset_id t
    then Some asset_id 
    else None  

end 

module Utl        = Asset_utl
module Validation = Asset_utl.Make_validation(App) 

let () = 
  let app = [] in 

  let url_content = "This is me" in
  let url = "http://test" in 
  let prv_key = Cry.Prv.make () in 

  let issue_asset = Utl.make_issue_asset ~url ~url_content ~prv_key () in 

  assert(Validation.validate_issue_asset ~url_content issue_asset app)

let () = 

  let asset_id = "This is a fake one" in 
  let transfer = Utl.make_transfer 
    ~prev_tx_id:App.prev_tx_id_str
    ~asset_id
    ~prv_key:App.owner_key
    ~dest_addr:(Utl.Binary (Cry.Prv.public_key App.receiver_key)) 
    () 
  in 
  let app = [asset_id] in

  assert(Validation.validate_transfer ~prev_tx_id:App.prev_tx_id_str transfer app)  

let () = 
  
  let asset_id = "This is a fake one" in 
  let accept_transfer = Utl.make_accept_transfer
    ~prev_tx_id:App.prev_tx_id_str 
    ~asset_id
    ~prv_key:App.receiver_key
    () 
  in 
  let app = [asset_id] in

  assert(Validation.validate_accept_transfer ~prev_tx_id:App.prev_tx_id_str accept_transfer app)  
