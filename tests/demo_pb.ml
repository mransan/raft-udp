[@@@ocaml.warning "-30"]

type tx = {
  hello_who : string;
}

and tx_mutable = {
  mutable hello_who : string;
}

let rec default_tx 
  ?hello_who:((hello_who:string) = "")
  () : tx  = {
  hello_who;
}

and default_tx_mutable () : tx_mutable = {
  hello_who = "";
}

let rec decode_tx d =
  let v = default_tx_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Bytes) -> (
      v.hello_who <- Pbrt.Decoder.string d;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(tx), field(1)", pk))
    )
    | Some (n, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:tx = Obj.magic v in
  v

let rec encode_tx (v:tx) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.hello_who encoder;
  ()

let rec pp_tx fmt (v:tx) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "hello_who" Pbrt.Pp.pp_string fmt v.hello_who;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()
