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

  type t = asset list 

  let find t asset_id =
    if List.mem asset_id t
    then Some asset_id 
    else None  
end 

module Utl = Asset_utl.Make(App) 

let alphabet = B58.make_alphabet "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

let () = 
  let app = [] in 

  let url_content = "This is me" in
  let url = "http://test" in 
  let prv_key = Cry.Prv.make () in 

  let issue_asset = Asset_utl.make_issue_asset ~url ~url_content ~prv_key () in 

  assert(Utl.validate_issue_asset ~url_content issue_asset app)


let () = 

  let asset_id = "This is a fake one" in 
  let transfer = Asset_utl.make_transfer 
    ~asset_id
    ~prv_key:App.owner_key
    ~dest_addr:(Asset_utl.Binary (Cry.Prv.public_key App.receiver_key)) 
    () 
  in 
  let app = [asset_id] in

  assert(Utl.validate_transfer transfer app)  
