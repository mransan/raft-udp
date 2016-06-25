(** asset.proto Generated Types and Encoding *)

(** {2 Types} *)

type asset = {
  url : string;
  hash : bytes;
}

type transfer = {
  asset : asset;
  dest_addr : bytes;
}

type accept_transfer = {
  transfer_id : bytes;
}

type tx =
  | Transfer of transfer
  | Accept_transfer of accept_transfer

type signed_tx = {
  tx : tx;
  signature : string;
}


(** {2 Default values} *)

val default_asset : 
  ?url:string ->
  ?hash:bytes ->
  unit ->
  asset
(** [default_asset ()] is the default value for type [asset] *)

val default_transfer : 
  ?asset:asset ->
  ?dest_addr:bytes ->
  unit ->
  transfer
(** [default_transfer ()] is the default value for type [transfer] *)

val default_accept_transfer : 
  ?transfer_id:bytes ->
  unit ->
  accept_transfer
(** [default_accept_transfer ()] is the default value for type [accept_transfer] *)

val default_tx : unit -> tx
(** [default_tx ()] is the default value for type [tx] *)

val default_signed_tx : 
  ?tx:tx ->
  ?signature:string ->
  unit ->
  signed_tx
(** [default_signed_tx ()] is the default value for type [signed_tx] *)


(** {2 Protobuf Decoding} *)

val decode_asset : Pbrt.Decoder.t -> asset
(** [decode_asset decoder] decodes a [asset] value from [decoder] *)

val decode_transfer : Pbrt.Decoder.t -> transfer
(** [decode_transfer decoder] decodes a [transfer] value from [decoder] *)

val decode_accept_transfer : Pbrt.Decoder.t -> accept_transfer
(** [decode_accept_transfer decoder] decodes a [accept_transfer] value from [decoder] *)

val decode_tx : Pbrt.Decoder.t -> tx
(** [decode_tx decoder] decodes a [tx] value from [decoder] *)

val decode_signed_tx : Pbrt.Decoder.t -> signed_tx
(** [decode_signed_tx decoder] decodes a [signed_tx] value from [decoder] *)


(** {2 Protobuf Toding} *)

val encode_asset : asset -> Pbrt.Encoder.t -> unit
(** [encode_asset v encoder] encodes [v] with the given [encoder] *)

val encode_transfer : transfer -> Pbrt.Encoder.t -> unit
(** [encode_transfer v encoder] encodes [v] with the given [encoder] *)

val encode_accept_transfer : accept_transfer -> Pbrt.Encoder.t -> unit
(** [encode_accept_transfer v encoder] encodes [v] with the given [encoder] *)

val encode_tx : tx -> Pbrt.Encoder.t -> unit
(** [encode_tx v encoder] encodes [v] with the given [encoder] *)

val encode_signed_tx : signed_tx -> Pbrt.Encoder.t -> unit
(** [encode_signed_tx v encoder] encodes [v] with the given [encoder] *)


(** {2 Formatters} *)

val pp_asset : Format.formatter -> asset -> unit 
(** [pp_asset v] formats v *)

val pp_transfer : Format.formatter -> transfer -> unit 
(** [pp_transfer v] formats v *)

val pp_accept_transfer : Format.formatter -> accept_transfer -> unit 
(** [pp_accept_transfer v] formats v *)

val pp_tx : Format.formatter -> tx -> unit 
(** [pp_tx v] formats v *)

val pp_signed_tx : Format.formatter -> signed_tx -> unit 
(** [pp_signed_tx v] formats v *)
