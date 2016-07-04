(** asset.proto Generated Types and Encoding *)


(** {2 Types} *)

type asset = {
  a_url : string;
  a_hash : string;
}

type issue_asset = {
  ia_asset : asset;
  ia_issuer_addr : string;
  ia_sig : string;
}

type transfer = {
  tr_asset_id : string;
  tr_dest_addr : string;
  tr_sig : string;
}

type accept_transfer = {
  at_asset_id : string;
  at_sig : string;
}

type tx =
  | Issue_asset of issue_asset
  | Transfer of transfer
  | Accept_transfer of accept_transfer


(** {2 Default values} *)

val default_asset : 
  ?a_url:string ->
  ?a_hash:string ->
  unit ->
  asset
(** [default_asset ()] is the default value for type [asset] *)

val default_issue_asset : 
  ?ia_asset:asset ->
  ?ia_issuer_addr:string ->
  ?ia_sig:string ->
  unit ->
  issue_asset
(** [default_issue_asset ()] is the default value for type [issue_asset] *)

val default_transfer : 
  ?tr_asset_id:string ->
  ?tr_dest_addr:string ->
  ?tr_sig:string ->
  unit ->
  transfer
(** [default_transfer ()] is the default value for type [transfer] *)

val default_accept_transfer : 
  ?at_asset_id:string ->
  ?at_sig:string ->
  unit ->
  accept_transfer
(** [default_accept_transfer ()] is the default value for type [accept_transfer] *)

val default_tx : unit -> tx
(** [default_tx ()] is the default value for type [tx] *)


(** {2 Protobuf Decoding} *)

val decode_asset : Pbrt.Decoder.t -> asset
(** [decode_asset decoder] decodes a [asset] value from [decoder] *)

val decode_issue_asset : Pbrt.Decoder.t -> issue_asset
(** [decode_issue_asset decoder] decodes a [issue_asset] value from [decoder] *)

val decode_transfer : Pbrt.Decoder.t -> transfer
(** [decode_transfer decoder] decodes a [transfer] value from [decoder] *)

val decode_accept_transfer : Pbrt.Decoder.t -> accept_transfer
(** [decode_accept_transfer decoder] decodes a [accept_transfer] value from [decoder] *)

val decode_tx : Pbrt.Decoder.t -> tx
(** [decode_tx decoder] decodes a [tx] value from [decoder] *)


(** {2 Protobuf Toding} *)

val encode_asset : asset -> Pbrt.Encoder.t -> unit
(** [encode_asset v encoder] encodes [v] with the given [encoder] *)

val encode_issue_asset : issue_asset -> Pbrt.Encoder.t -> unit
(** [encode_issue_asset v encoder] encodes [v] with the given [encoder] *)

val encode_transfer : transfer -> Pbrt.Encoder.t -> unit
(** [encode_transfer v encoder] encodes [v] with the given [encoder] *)

val encode_accept_transfer : accept_transfer -> Pbrt.Encoder.t -> unit
(** [encode_accept_transfer v encoder] encodes [v] with the given [encoder] *)

val encode_tx : tx -> Pbrt.Encoder.t -> unit
(** [encode_tx v encoder] encodes [v] with the given [encoder] *)


(** {2 Formatters} *)

val pp_asset : Format.formatter -> asset -> unit 
(** [pp_asset v] formats v *)

val pp_issue_asset : Format.formatter -> issue_asset -> unit 
(** [pp_issue_asset v] formats v *)

val pp_transfer : Format.formatter -> transfer -> unit 
(** [pp_transfer v] formats v *)

val pp_accept_transfer : Format.formatter -> accept_transfer -> unit 
(** [pp_accept_transfer v] formats v *)

val pp_tx : Format.formatter -> tx -> unit 
(** [pp_tx v] formats v *)
