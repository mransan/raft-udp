(** Utility function to create and validate transactions 
  *)

val make_issue_asset : 
  url:string -> 
  url_content:string -> 
  prv_key:Raft_cry.Prv.t -> 
  unit -> 
  Asset_pb.issue_asset 

module type App_sig = sig 

  type asset 

  val owner : asset -> string option 
  (** [owner asset] returns the address (ie base58 encoded public key) 
      of the asset owner. 
      
      {ul 
      {- [None] is returned if the asset has been transfered but 
         not accepted yet.}
      {- [Some addr] is returned where [addr] is the base58 encoded 
         binary public key of the owner.}
      } 
    *)

  val receiver : asset -> string option 
  (** [receiver asset] returns the address (ie base58 encoded public key)
      of the receiver of the asset. 

      {ul 
      {- [None] is returned if the asset is not pending an accept}
      {- [Some addr] is returned where [addr] is the base58 
         encoded public key.} 
      }
    *)

  type t

  val find : t -> string -> asset option 
  (** [find app asset_id] returns 
      
      {ul
      {- [None] is no asset with [asset_id] has been issued previously}
      {- [Some asset] the asset identified by [asset_id]}
      }
    *)
end 
  
module Make(App:App_sig) : sig 

  val validate_asset : 
    Asset_pb.asset -> 
    url_content:string -> 
    App.t -> 
    bool 

  val validate_issue_asset : 
    Asset_pb.issue_asset -> 
    url_content:string -> 
    App.t -> 
    bool 
end 
