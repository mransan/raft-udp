[@@@ocaml.warning "-27-30-39"]

type asset = {
  url : string;
  hash : bytes;
}

and asset_mutable = {
  mutable url : string;
  mutable hash : bytes;
}

type transfer = {
  asset : asset;
  dest_addr : bytes;
}

and transfer_mutable = {
  mutable asset : asset;
  mutable dest_addr : bytes;
}

type accept_transfer = {
  transfer_id : bytes;
}

and accept_transfer_mutable = {
  mutable transfer_id : bytes;
}

type tx =
  | Transfer of transfer
  | Accept_transfer of accept_transfer

type signed_tx = {
  tx : tx;
  signature : string;
}

and signed_tx_mutable = {
  mutable tx : tx;
  mutable signature : string;
}

let rec default_asset 
  ?url:((url:string) = "")
  ?hash:((hash:bytes) = Bytes.create 64)
  () : asset  = {
  url;
  hash;
}

and default_asset_mutable () : asset_mutable = {
  url = "";
  hash = Bytes.create 64;
}

let rec default_transfer 
  ?asset:((asset:asset) = default_asset ())
  ?dest_addr:((dest_addr:bytes) = Bytes.create 64)
  () : transfer  = {
  asset;
  dest_addr;
}

and default_transfer_mutable () : transfer_mutable = {
  asset = default_asset ();
  dest_addr = Bytes.create 64;
}

let rec default_accept_transfer 
  ?transfer_id:((transfer_id:bytes) = Bytes.create 64)
  () : accept_transfer  = {
  transfer_id;
}

and default_accept_transfer_mutable () : accept_transfer_mutable = {
  transfer_id = Bytes.create 64;
}

let rec default_tx () : tx = Transfer (default_transfer ())

let rec default_signed_tx 
  ?tx:((tx:tx) = default_tx ())
  ?signature:((signature:string) = "")
  () : signed_tx  = {
  tx;
  signature;
}

and default_signed_tx_mutable () : signed_tx_mutable = {
  tx = default_tx ();
  signature = "";
}

let rec decode_asset d =
  let v = default_asset_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Bytes) -> (
      v.url <- Pbrt.Decoder.string d;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(asset), field(1)", pk))
    )
    | Some (2, Pbrt.Bytes) -> (
      v.hash <- Pbrt.Decoder.bytes d;
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(asset), field(2)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:asset = Obj.magic v in
  v

let rec decode_transfer d =
  let v = default_transfer_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Bytes) -> (
      v.asset <- decode_asset (Pbrt.Decoder.nested d);
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(transfer), field(1)", pk))
    )
    | Some (2, Pbrt.Bytes) -> (
      v.dest_addr <- Pbrt.Decoder.bytes d;
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(transfer), field(2)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:transfer = Obj.magic v in
  v

let rec decode_accept_transfer d =
  let v = default_accept_transfer_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Bytes) -> (
      v.transfer_id <- Pbrt.Decoder.bytes d;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(accept_transfer), field(1)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:accept_transfer = Obj.magic v in
  v

let rec decode_tx d = 
  let rec loop () = 
    let ret:tx = match Pbrt.Decoder.key d with
      | None -> failwith "None of the known key is found"
      | Some (1, _) -> Transfer (decode_transfer (Pbrt.Decoder.nested d))
      | Some (2, _) -> Accept_transfer (decode_accept_transfer (Pbrt.Decoder.nested d))
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

let rec decode_signed_tx d =
  let v = default_signed_tx_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Bytes) -> (
      v.tx <- decode_tx (Pbrt.Decoder.nested d);
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(signed_tx), field(1)", pk))
    )
    | Some (2, Pbrt.Bytes) -> (
      v.signature <- Pbrt.Decoder.string d;
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(signed_tx), field(2)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:signed_tx = Obj.magic v in
  v

let rec encode_asset (v:asset) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.url encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.bytes v.hash encoder;
  ()

let rec encode_transfer (v:transfer) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_asset v.asset) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.bytes v.dest_addr encoder;
  ()

let rec encode_accept_transfer (v:accept_transfer) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.bytes v.transfer_id encoder;
  ()

let rec encode_tx (v:tx) encoder = 
  match v with
  | Transfer x -> (
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_transfer x) encoder;
  )
  | Accept_transfer x -> (
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_accept_transfer x) encoder;
  )

let rec encode_signed_tx (v:signed_tx) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_tx v.tx) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.signature encoder;
  ()

let rec pp_asset fmt (v:asset) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "url" Pbrt.Pp.pp_string fmt v.url;
    Pbrt.Pp.pp_record_field "hash" Pbrt.Pp.pp_bytes fmt v.hash;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_transfer fmt (v:transfer) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "asset" pp_asset fmt v.asset;
    Pbrt.Pp.pp_record_field "dest_addr" Pbrt.Pp.pp_bytes fmt v.dest_addr;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_accept_transfer fmt (v:accept_transfer) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "transfer_id" Pbrt.Pp.pp_bytes fmt v.transfer_id;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_tx fmt (v:tx) =
  match v with
  | Transfer x -> Format.fprintf fmt "@[Transfer(%a)@]" pp_transfer x
  | Accept_transfer x -> Format.fprintf fmt "@[Accept_transfer(%a)@]" pp_accept_transfer x

let rec pp_signed_tx fmt (v:signed_tx) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "tx" pp_tx fmt v.tx;
    Pbrt.Pp.pp_record_field "signature" Pbrt.Pp.pp_string fmt v.signature;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()
