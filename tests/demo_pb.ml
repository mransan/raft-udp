[@@@ocaml.warning "-27-30-39"]

type tx = {
  counter_value : int;
  process_id : int;
}

and tx_mutable = {
  mutable counter_value : int;
  mutable process_id : int;
}

let rec default_tx 
  ?counter_value:((counter_value:int) = 0)
  ?process_id:((process_id:int) = 0)
  () : tx  = {
  counter_value;
  process_id;
}

and default_tx_mutable () : tx_mutable = {
  counter_value = 0;
  process_id = 0;
}

let rec decode_tx d =
  let v = default_tx_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Varint) -> (
      v.counter_value <- Pbrt.Decoder.int_as_varint d;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(tx), field(1)", pk))
    )
    | Some (2, Pbrt.Varint) -> (
      v.process_id <- Pbrt.Decoder.int_as_varint d;
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
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.counter_value encoder;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.process_id encoder;
  ()

let rec pp_tx fmt (v:tx) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "counter_value" Pbrt.Pp.pp_int fmt v.counter_value;
    Pbrt.Pp.pp_record_field "process_id" Pbrt.Pp.pp_int fmt v.process_id;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()
