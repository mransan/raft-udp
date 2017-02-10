[@@@ocaml.warning "-27-30-39"]

type asset = {
  a_url : string;
  a_hash : string;
}

and asset_mutable = {
  mutable a_url : string;
  mutable a_hash : string;
}

type issue_asset = {
  ia_asset : asset;
  ia_issuer_addr : string;
  ia_sig : string;
}

and issue_asset_mutable = {
  mutable ia_asset : asset;
  mutable ia_issuer_addr : string;
  mutable ia_sig : string;
}

type transfer = {
  tr_asset_id : string;
  tr_dest_addr : string;
  tr_sig : string;
}

and transfer_mutable = {
  mutable tr_asset_id : string;
  mutable tr_dest_addr : string;
  mutable tr_sig : string;
}

type accept_transfer = {
  at_asset_id : string;
  at_sig : string;
}

and accept_transfer_mutable = {
  mutable at_asset_id : string;
  mutable at_sig : string;
}

type tx =
  | Issue_asset of issue_asset
  | Transfer of transfer
  | Accept_transfer of accept_transfer

let rec default_asset 
  ?a_url:((a_url:string) = "")
  ?a_hash:((a_hash:string) = "")
  () : asset  = {
  a_url;
  a_hash;
}

and default_asset_mutable () : asset_mutable = {
  a_url = "";
  a_hash = "";
}

let rec default_issue_asset 
  ?ia_asset:((ia_asset:asset) = default_asset ())
  ?ia_issuer_addr:((ia_issuer_addr:string) = "")
  ?ia_sig:((ia_sig:string) = "")
  () : issue_asset  = {
  ia_asset;
  ia_issuer_addr;
  ia_sig;
}

and default_issue_asset_mutable () : issue_asset_mutable = {
  ia_asset = default_asset ();
  ia_issuer_addr = "";
  ia_sig = "";
}

let rec default_transfer 
  ?tr_asset_id:((tr_asset_id:string) = "")
  ?tr_dest_addr:((tr_dest_addr:string) = "")
  ?tr_sig:((tr_sig:string) = "")
  () : transfer  = {
  tr_asset_id;
  tr_dest_addr;
  tr_sig;
}

and default_transfer_mutable () : transfer_mutable = {
  tr_asset_id = "";
  tr_dest_addr = "";
  tr_sig = "";
}

let rec default_accept_transfer 
  ?at_asset_id:((at_asset_id:string) = "")
  ?at_sig:((at_sig:string) = "")
  () : accept_transfer  = {
  at_asset_id;
  at_sig;
}

and default_accept_transfer_mutable () : accept_transfer_mutable = {
  at_asset_id = "";
  at_sig = "";
}

let rec default_tx () : tx = Issue_asset (default_issue_asset ())

let rec decode_asset d =
  let v = default_asset_mutable () in
  let a_hash_is_set = ref false in
  let a_url_is_set = ref false in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Bytes) -> (
      v.a_url <- Pbrt.Decoder.string d; a_url_is_set := true;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(asset), field(1)", pk))
    )
    | Some (2, Pbrt.Bytes) -> (
      v.a_hash <- Pbrt.Decoder.string d; a_hash_is_set := true;
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(asset), field(2)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  begin if not !a_hash_is_set then raise Protobuf.Decoder.(Failure (Missing_field "a_hash")) end;
  begin if not !a_url_is_set then raise Protobuf.Decoder.(Failure (Missing_field "a_url")) end;
  let v:asset = Obj.magic v in
  v

let rec decode_issue_asset d =
  let v = default_issue_asset_mutable () in
  let ia_sig_is_set = ref false in
  let ia_issuer_addr_is_set = ref false in
  let ia_asset_is_set = ref false in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Bytes) -> (
      v.ia_asset <- decode_asset (Pbrt.Decoder.nested d); ia_asset_is_set := true;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(issue_asset), field(1)", pk))
    )
    | Some (2, Pbrt.Bytes) -> (
      v.ia_issuer_addr <- Pbrt.Decoder.string d; ia_issuer_addr_is_set := true;
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(issue_asset), field(2)", pk))
    )
    | Some (3, Pbrt.Bytes) -> (
      v.ia_sig <- Pbrt.Decoder.string d; ia_sig_is_set := true;
      loop ()
    )
    | Some (3, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(issue_asset), field(3)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  begin if not !ia_sig_is_set then raise Protobuf.Decoder.(Failure (Missing_field "ia_sig")) end;
  begin if not !ia_issuer_addr_is_set then raise Protobuf.Decoder.(Failure (Missing_field "ia_issuer_addr")) end;
  begin if not !ia_asset_is_set then raise Protobuf.Decoder.(Failure (Missing_field "ia_asset")) end;
  let v:issue_asset = Obj.magic v in
  v

let rec decode_transfer d =
  let v = default_transfer_mutable () in
  let tr_sig_is_set = ref false in
  let tr_dest_addr_is_set = ref false in
  let tr_asset_id_is_set = ref false in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Bytes) -> (
      v.tr_asset_id <- Pbrt.Decoder.string d; tr_asset_id_is_set := true;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(transfer), field(1)", pk))
    )
    | Some (2, Pbrt.Bytes) -> (
      v.tr_dest_addr <- Pbrt.Decoder.string d; tr_dest_addr_is_set := true;
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(transfer), field(2)", pk))
    )
    | Some (3, Pbrt.Bytes) -> (
      v.tr_sig <- Pbrt.Decoder.string d; tr_sig_is_set := true;
      loop ()
    )
    | Some (3, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(transfer), field(3)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  begin if not !tr_sig_is_set then raise Protobuf.Decoder.(Failure (Missing_field "tr_sig")) end;
  begin if not !tr_dest_addr_is_set then raise Protobuf.Decoder.(Failure (Missing_field "tr_dest_addr")) end;
  begin if not !tr_asset_id_is_set then raise Protobuf.Decoder.(Failure (Missing_field "tr_asset_id")) end;
  let v:transfer = Obj.magic v in
  v

let rec decode_accept_transfer d =
  let v = default_accept_transfer_mutable () in
  let at_sig_is_set = ref false in
  let at_asset_id_is_set = ref false in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Bytes) -> (
      v.at_asset_id <- Pbrt.Decoder.string d; at_asset_id_is_set := true;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(accept_transfer), field(1)", pk))
    )
    | Some (2, Pbrt.Bytes) -> (
      v.at_sig <- Pbrt.Decoder.string d; at_sig_is_set := true;
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(accept_transfer), field(2)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  begin if not !at_sig_is_set then raise Protobuf.Decoder.(Failure (Missing_field "at_sig")) end;
  begin if not !at_asset_id_is_set then raise Protobuf.Decoder.(Failure (Missing_field "at_asset_id")) end;
  let v:accept_transfer = Obj.magic v in
  v

let rec decode_tx d = 
  let rec loop () = 
    let ret:tx = match Pbrt.Decoder.key d with
      | None -> failwith "None of the known key is found"
      | Some (1, _) -> Issue_asset (decode_issue_asset (Pbrt.Decoder.nested d))
      | Some (2, _) -> Transfer (decode_transfer (Pbrt.Decoder.nested d))
      | Some (3, _) -> Accept_transfer (decode_accept_transfer (Pbrt.Decoder.nested d))
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

let rec encode_asset (v:asset) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.a_url encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.a_hash encoder;
  ()

let rec encode_issue_asset (v:issue_asset) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_asset v.ia_asset) encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.ia_issuer_addr encoder;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.ia_sig encoder;
  ()

let rec encode_transfer (v:transfer) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.tr_asset_id encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.tr_dest_addr encoder;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.tr_sig encoder;
  ()

let rec encode_accept_transfer (v:accept_transfer) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.at_asset_id encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.at_sig encoder;
  ()

let rec encode_tx (v:tx) encoder = 
  match v with
  | Issue_asset x -> (
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_issue_asset x) encoder;
  )
  | Transfer x -> (
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_transfer x) encoder;
  )
  | Accept_transfer x -> (
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_accept_transfer x) encoder;
  )

let rec pp_asset fmt (v:asset) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "a_url" Pbrt.Pp.pp_string fmt v.a_url;
    Pbrt.Pp.pp_record_field "a_hash" Pbrt.Pp.pp_string fmt v.a_hash;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_issue_asset fmt (v:issue_asset) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "ia_asset" pp_asset fmt v.ia_asset;
    Pbrt.Pp.pp_record_field "ia_issuer_addr" Pbrt.Pp.pp_string fmt v.ia_issuer_addr;
    Pbrt.Pp.pp_record_field "ia_sig" Pbrt.Pp.pp_string fmt v.ia_sig;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_transfer fmt (v:transfer) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "tr_asset_id" Pbrt.Pp.pp_string fmt v.tr_asset_id;
    Pbrt.Pp.pp_record_field "tr_dest_addr" Pbrt.Pp.pp_string fmt v.tr_dest_addr;
    Pbrt.Pp.pp_record_field "tr_sig" Pbrt.Pp.pp_string fmt v.tr_sig;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_accept_transfer fmt (v:accept_transfer) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "at_asset_id" Pbrt.Pp.pp_string fmt v.at_asset_id;
    Pbrt.Pp.pp_record_field "at_sig" Pbrt.Pp.pp_string fmt v.at_sig;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_tx fmt (v:tx) =
  match v with
  | Issue_asset x -> Format.fprintf fmt "@[Issue_asset(%a)@]" pp_issue_asset x
  | Transfer x -> Format.fprintf fmt "@[Transfer(%a)@]" pp_transfer x
  | Accept_transfer x -> Format.fprintf fmt "@[Accept_transfer(%a)@]" pp_accept_transfer x
