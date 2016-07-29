(** Utility function to create and validate transactions 
  *)
  
type tx_id = string 

val make_issue_asset : 
  url:string -> 
  url_content:string -> 
  prv_key:Raft_cry.Prv.t -> 
  unit -> 
  (Asset_pb.issue_asset * tx_id) 

type dest_addr = 
  | Binary of Raft_cry.Pub.t 
  | Text   of string 

val make_transfer : 
  prev_tx_id:string ->
  asset_id:string -> 
  dest_addr:dest_addr -> 
  prv_key:Raft_cry.Prv.t -> 
  unit -> 
  (Asset_pb.transfer * tx_id) 

val make_accept_transfer : 
  prev_tx_id:string -> 
  asset_id:string -> 
  prv_key:Raft_cry.Prv.t ->
  unit -> 
  (Asset_pb.accept_transfer * tx_id)

val pub_key_of_addr : string -> Raft_cry.Pub.t 

module type App_sig = sig 

  type asset 

  val owner : asset -> Raft_cry.Pub.t option 
  (** [owner asset] returns the address (ie base58 encoded public key) 
      of the asset owner. 
      
      {ul 
      {- [None] is returned if the asset has been transfered but 
         not accepted yet.}
      {- [Some addr] is returned where [addr] is the public key of the owner}
      } 
    *)

  val receiver : asset -> Raft_cry.Pub.t option 
  (** [receiver asset] returns the address (ie base58 encoded public key)
      of the receiver of the asset. 

      {ul 
      {- [None] is returned if the asset is not pending an accept}
      {- [Some addr] is returned where [addr] is the public key of the receiver}
      }
    *)

  val prev_tx_id : asset -> string 
  (** [prev_tx_id asset] returns the id of the previous transaction 
      associated with [asset] in binary format. (ie not base58 encoded)
    *) 

  type t

  val find : t -> string -> asset option 
  (** [find app asset_id] returns 
      
      {ul
      {- [None] is no asset with [asset_id] has been issued previously}
      {- [Some asset] the asset identified by [asset_id]}
      }
    *)
end (* App_sig *) 
  
module Make_validation(App:App_sig) : sig 

  type 'a ok_result = {
    tx_id : tx_id; 
    ok_data : 'a; 
  }
  
  type validation_error =
    | Invalid_asset_id
      (** 
       The asset hash in the Issue_asset transaction is not valid. This 
       can be due 
       {ul 
       {- url content is not matching }
       {- invalid sha256 hashing }
       {- invalid Base 58 encoding (different alphabet?)} 
       }
       *) 

    | Duplicate_asset of string 
      (** 
       The asset being issued already exists.
       *) 

    | Invalid_signature of string * string * string * (string option) (* (id, signature, public key) *)  
      (** 
       The signature associated with the transaction is invalid. This can 
       be due to:
       {ul
       {- Invalid id computation}
       {- Invalid private key}
       {- invalid Base 58 encoding (different alphabet?)}
       }
       *)

    | Unknown_asset of string 
      (** 
       The asset being transfered/accepted does not exists (ie has never 
       been issued). 
       *)

    | Attempt_to_transfer_in_transfer_asset of string  
      (** 
       The transfer transaction cannot be completed since the corresponding 
       asset is already in transfer 
       *) 

    | Asset_not_in_transfer of string 
      (** 
       An Accept_transfer is rejected since the asset is currently not 
       in transfer. 
       *) 

  val string_of_validation_error : validation_error -> string 

  type 'a validation_result = 
    | Validation_ok of 'a ok_result  
    | Validation_error of validation_error  

  type issue_asset_ok = Raft_cry.Pub.t  
  (** The validation return the public key of the owner of the 
      asset being issued. 
   *)

  val validate_issue_asset : 
    Asset_pb.issue_asset -> 
    url_content:string -> 
    App.t -> 
    issue_asset_ok validation_result 
  
  type transfer_ok = {
    tr_asset : App.asset; 
    tr_receiver : Raft_cry.Pub.t 
  }

  val validate_transfer: 
    Asset_pb.transfer -> 
    App.t -> 
    transfer_ok validation_result 
  
  type accept_transfer_ok = {
    at_asset : App.asset; 
    at_owner : Raft_cry.Pub.t; 
  }

  val validate_accept_transfer: 
    Asset_pb.accept_transfer -> 
    App.t -> 
    accept_transfer_ok validation_result 

end (* Make_validation *) 
