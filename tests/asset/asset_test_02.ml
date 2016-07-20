module Pb = Asset_pb
module Cry = Raft_cry

open Lwt.Infix

let random_string ?n:(n = 10) () = 
  String.init n (fun _ -> 
    char_of_int @@ 64 + Random.int 26 
  )

let random_url () = 
  let url = random_string () in 
  Asset_app.content_of_url url 
  >|=(fun url_content -> (url, url_content)) 

(* 
 * Utility function to get the previous tx id 
 *)
let prev_tx_id app asset_id = 
  match Asset_app.App.find app asset_id with
  | None -> failwith @@ Printf.sprintf "** Missing asset_id: %s" asset_id
  | Some asset -> Asset_app.App.prev_tx_id asset 


let main () =
  (*
   * This unit test makes sure that the Asset_app module handles 
   * correctly the issuance,transfer and acceptance of the transfer correctly. 
   * 
   * After the 3 transactions are executed we verify that the 
   * asset is owned by the correct key. 
   *)

  let app = Asset_app.make () in 
  let owner = Cry.Prv.make () in 
  let receiver = Cry.Prv.make () in

  random_url () 
  >>=(fun (url, url_content) ->
    let issue_asset = 
      Asset_utl.make_issue_asset ~url ~url_content ~prv_key:owner ()
    in
    Asset_app.handle_tx app (Pb.Issue_asset issue_asset) 
    >|=(fun app -> 
      (app, issue_asset.Pb.ia_asset.Pb.a_hash)
    )
  ) 
  >|=(fun ((app, _) as r) -> 
    Printf.printf "- App after issue asset:\n> %s\n" (Asset_app.App.show app); 
    r
  )
  >>=(fun (app, asset_id) ->
    let prev_tx_id = prev_tx_id app asset_id in 
    let transfer = Asset_utl.make_transfer 
      ~prev_tx_id
      ~asset_id
      ~dest_addr:(Asset_utl.Binary (Cry.Prv.public_key receiver))
      ~prv_key:owner
      ()
    in 
    Asset_app.handle_tx app (Pb.Transfer transfer)
    >|=(fun app -> (app, asset_id))
  ) 
  >|=(fun ((app, _) as r)-> 
    Printf.printf "- App after transfer asset:\n> %s\n" (Asset_app.App.show app); 
    r
  )
  >>=(fun (app, asset_id) -> 
    let prev_tx_id = prev_tx_id app asset_id in 
    let accept_transfer = Asset_utl.make_accept_transfer 
      ~prev_tx_id
      ~asset_id
      ~prv_key:receiver
      ()
    in 
    Asset_app.handle_tx app (Pb.Accept_transfer accept_transfer) 
    >|= (fun app -> (app, asset_id))
  ) 
  >|= (fun (app, asset_id) -> 
    Printf.printf "- App after accept transfer :\n> %s\n" (Asset_app.App.show app); 
    match Asset_app.App.find app asset_id with
    | None -> assert(false)
    | Some asset -> 
       assert(Asset_app.App.owner asset = Some (Cry.Prv.public_key receiver))
  ) 

let () = Lwt_main.run (main ()) 
