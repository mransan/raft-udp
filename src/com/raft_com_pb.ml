[@@@ocaml.warning "-27-30-39"]

type tx = {
  tx_id : string;
  tx_data : bytes;
}

and tx_mutable = {
  mutable tx_id : string;
  mutable tx_data : bytes;
}

let rec default_tx 
  ?tx_id:((tx_id:string) = "")
  ?tx_data:((tx_data:bytes) = Bytes.create 64)
  () : tx  = {
  tx_id;
  tx_data;
}

and default_tx_mutable () : tx_mutable = {
  tx_id = "";
  tx_data = Bytes.create 64;
}

let rec decode_tx d =
  let v = default_tx_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Bytes) -> (
      v.tx_id <- Pbrt.Decoder.string d;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(tx), field(1)", pk))
    )
    | Some (2, Pbrt.Bytes) -> (
      v.tx_data <- Pbrt.Decoder.bytes d;
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(tx), field(2)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:tx = Obj.magic v in
  v

let rec encode_tx (v:tx) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.tx_id encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.bytes v.tx_data encoder;
  ()

let rec pp_tx fmt (v:tx) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "tx_id" Pbrt.Pp.pp_string fmt v.tx_id;
    Pbrt.Pp.pp_record_field "tx_data" Pbrt.Pp.pp_bytes fmt v.tx_data;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()
