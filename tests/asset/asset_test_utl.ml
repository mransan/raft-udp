module Pb = Asset_pb
module Cry = Raft_cry 
open Lwt.Infix

let random_string ?n:(n = 10) () = 
  String.init n (fun _ -> 
    char_of_int @@ 64 + Random.int 26 
  )


type test_env = {
  keys : Cry.Prv.t list;
}

let make_env ~nb_of_keys () = 
  let rec aux = function
    | 0 -> [] 
    | n -> (Cry.Prv.make ()) :: (aux @@ n - 1) 
  in 
  {keys = aux nb_of_keys}

type asset_info = {
  id: string; 
  prev_tx_id: string; 
  prv_key : Cry.Prv.t;
}

type test_exec = 
  | Not_started
  | Issued of asset_info 
  | In_transfer of asset_info 
  | Accepted of asset_info  

type test = {
  remaining_transfers : int; 
  execution : test_exec; 
  env : test_env;
}

let make_test ~env ~nb_of_transfers () = {
  remaining_transfers = nb_of_transfers; 
  execution = Not_started; 
  env; 
}

let get_random_key {keys; } = 
  let len = List.length keys in 
  List.nth keys (Random.int len) 

let is_done {remaining_transfers; execution; _} = 
  match execution, remaining_transfers with
  | Accepted _, rt when rt <= 0 -> true
  | _ -> false

module type App_sig = sig 

  type t 

  val content_of_url : string -> string Lwt.t 

  val handle_tx : logger:Lwt_log_core.logger -> t -> Asset_pb.tx -> (t, string) result Lwt.t  

end 

let handle_error ~tx_type = function 
  | Ok x -> Lwt.return x 
  | Error error_msg -> 
    Lwt.fail_with @@ Printf.sprintf 
      "Error handling tx in %s, details: %s" tx_type error_msg

module Make(App:App_sig) = struct 

  let random_url () = 
    let url = random_string () in 
    App.content_of_url url 
    >|=(fun url_content -> (url, url_content)) 

  let execute_test ~logger ({remaining_transfers; env; execution} as test) app = 
    match execution, remaining_transfers with
    | Accepted _ , rt when rt <= 0 -> 
      (* Nothing to be done here the test should have not been 
       * executed 
       *) 
      Lwt.fail_with "Test is done"
  
    | Not_started, _ -> 
      random_url () 
      >>=(fun (url, url_content) ->
        let prv_key = get_random_key env in 
        let issue_asset, prev_tx_id = 
          Asset_utl.make_issue_asset ~url ~url_content ~prv_key ()
        in
        App.handle_tx ~logger app (Pb.Issue_asset issue_asset) 
        >>= handle_error ~tx_type:"Issue_asset"
        >|=(fun app -> 
          let asset_info = {
            id = issue_asset.Pb.ia_asset.Pb.a_id; 
            prev_tx_id; 
            prv_key; 
          } in
          let test = {test with execution = Issued asset_info; } in  
          (test, app)
        )
      )
  
    | Issued asset_info, _ 
    | Accepted asset_info, _ -> 
      let {id; prev_tx_id; prv_key} = asset_info in 
      let receiver = get_random_key env in 
      let transfer, prev_tx_id = Asset_utl.make_transfer 
        ~prev_tx_id
        ~asset_id:id
        ~dest_addr:(Asset_utl.Binary (Cry.Prv.public_key receiver))
        ~prv_key
        ()
      in 
      App.handle_tx ~logger app (Pb.Transfer transfer)
      >>= handle_error ~tx_type:"Transfer"
      >|=(fun app ->
        let asset_info = {asset_info with prev_tx_id; prv_key = receiver; } in
        let test = {test with 
          remaining_transfers = remaining_transfers - 1; 
          execution = In_transfer asset_info 
        } in 
        (test, app)
      )
  
     | In_transfer asset_info, _ -> 
       let {id; prev_tx_id; prv_key} = asset_info in 
       let accept_transfer, prev_tx_id = Asset_utl.make_accept_transfer 
          ~prev_tx_id
          ~asset_id:id 
          ~prv_key
          ()
        in 
        App.handle_tx ~logger app (Pb.Accept_transfer accept_transfer) 
        >>=handle_error ~tx_type:"Accept_transfer"
        >|=(fun app ->
          (*
          match App.find app id with
          | None -> assert(false)
          | Some asset -> 
            assert(Asset_app.App.owner asset = Some (Cry.Prv.public_key prv_key));
          *)
            let asset_info = {asset_info with prev_tx_id} in 
            let test = {test with execution = Accepted asset_info} in 
            (test, app) 
        ) 

end (* Make *)
