[@@@ocaml.warning "-27-30-39"]

type app_request_txs = {
  txs : Raft_com_pb.tx list;
}

and app_request_txs_mutable = {
  mutable txs : Raft_com_pb.tx list;
}

type app_request =
  | Commit_txs of app_request_txs

type app_response_validation_failure = {
  error_message : string;
  error_code : int;
}

and app_response_validation_failure_mutable = {
  mutable error_message : string;
  mutable error_code : int;
}

type app_response_validation_result =
  | Validation_success
  | Validation_failure of app_response_validation_failure

and app_response_validation = {
  tx_id : string;
  result : app_response_validation_result;
}

and app_response_validation_mutable = {
  mutable tx_id : string;
  mutable result : app_response_validation_result;
}

type app_response_validations = {
  validations : app_response_validation list;
}

and app_response_validations_mutable = {
  mutable validations : app_response_validation list;
}

type app_response =
  | Committed_txs of app_response_validations

let rec default_app_request_txs 
  ?txs:((txs:Raft_com_pb.tx list) = [])
  () : app_request_txs  = {
  txs;
}

and default_app_request_txs_mutable () : app_request_txs_mutable = {
  txs = [];
}

let rec default_app_request () : app_request = Commit_txs (default_app_request_txs ())

let rec default_app_response_validation_failure 
  ?error_message:((error_message:string) = "")
  ?error_code:((error_code:int) = 0)
  () : app_response_validation_failure  = {
  error_message;
  error_code;
}

and default_app_response_validation_failure_mutable () : app_response_validation_failure_mutable = {
  error_message = "";
  error_code = 0;
}

let rec default_app_response_validation_result (): app_response_validation_result = Validation_success

and default_app_response_validation 
  ?tx_id:((tx_id:string) = "")
  ?result:((result:app_response_validation_result) = Validation_success)
  () : app_response_validation  = {
  tx_id;
  result;
}

and default_app_response_validation_mutable () : app_response_validation_mutable = {
  tx_id = "";
  result = Validation_success;
}

let rec default_app_response_validations 
  ?validations:((validations:app_response_validation list) = [])
  () : app_response_validations  = {
  validations;
}

and default_app_response_validations_mutable () : app_response_validations_mutable = {
  validations = [];
}

let rec default_app_response () : app_response = Committed_txs (default_app_response_validations ())

let rec decode_app_request_txs d =
  let v = default_app_request_txs_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
      v.txs <- List.rev v.txs;
    )
    | Some (1, Pbrt.Bytes) -> (
      v.txs <- (Raft_com_pb.decode_tx (Pbrt.Decoder.nested d)) :: v.txs;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(app_request_txs), field(1)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:app_request_txs = Obj.magic v in
  v

let rec decode_app_request d = 
  let rec loop () = 
    let ret:app_request = match Pbrt.Decoder.key d with
      | None -> failwith "None of the known key is found"
      | Some (4, _) -> Commit_txs (decode_app_request_txs (Pbrt.Decoder.nested d))
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

let rec decode_app_response_validation_failure d =
  let v = default_app_response_validation_failure_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Bytes) -> (
      v.error_message <- Pbrt.Decoder.string d;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(app_response_validation_failure), field(1)", pk))
    )
    | Some (2, Pbrt.Varint) -> (
      v.error_code <- Pbrt.Decoder.int_as_varint d;
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(app_response_validation_failure), field(2)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:app_response_validation_failure = Obj.magic v in
  v

let rec decode_app_response_validation_result d = 
  let rec loop () = 
    let ret:app_response_validation_result = match Pbrt.Decoder.key d with
      | None -> failwith "None of the known key is found"
      | Some (2, _) -> (Pbrt.Decoder.empty_nested d ; Validation_success)
      | Some (3, _) -> Validation_failure (decode_app_response_validation_failure (Pbrt.Decoder.nested d))
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

and decode_app_response_validation d =
  let v = default_app_response_validation_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Bytes) -> (
      v.tx_id <- Pbrt.Decoder.string d;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(app_response_validation), field(1)", pk))
    )
    | Some (2, Pbrt.Bytes) -> (
      Pbrt.Decoder.empty_nested d;
      v.result <- Validation_success;
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(app_response_validation), field(2)", pk))
    )
    | Some (3, Pbrt.Bytes) -> (
      v.result <- Validation_failure (decode_app_response_validation_failure (Pbrt.Decoder.nested d));
      loop ()
    )
    | Some (3, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(app_response_validation), field(3)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:app_response_validation = Obj.magic v in
  v

let rec decode_app_response_validations d =
  let v = default_app_response_validations_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
      v.validations <- List.rev v.validations;
    )
    | Some (1, Pbrt.Bytes) -> (
      v.validations <- (decode_app_response_validation (Pbrt.Decoder.nested d)) :: v.validations;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(app_response_validations), field(1)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:app_response_validations = Obj.magic v in
  v

let rec decode_app_response d = 
  let rec loop () = 
    let ret:app_response = match Pbrt.Decoder.key d with
      | None -> failwith "None of the known key is found"
      | Some (5, _) -> Committed_txs (decode_app_response_validations (Pbrt.Decoder.nested d))
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

let rec encode_app_request_txs (v:app_request_txs) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (Raft_com_pb.encode_tx x) encoder;
  ) v.txs;
  ()

let rec encode_app_request (v:app_request) encoder = 
  match v with
  | Commit_txs x -> (
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_app_request_txs x) encoder;
  )

let rec encode_app_response_validation_failure (v:app_response_validation_failure) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.error_message encoder;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.error_code encoder;
  ()

let rec encode_app_response_validation_result (v:app_response_validation_result) encoder = 
  match v with
  | Validation_success -> (
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  )
  | Validation_failure x -> (
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_app_response_validation_failure x) encoder;
  )

and encode_app_response_validation (v:app_response_validation) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.tx_id encoder;
  (
    match v.result with
    | Validation_success -> (
      Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
      Pbrt.Encoder.empty_nested encoder
    )
    | Validation_failure x -> (
      Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
      Pbrt.Encoder.nested (encode_app_response_validation_failure x) encoder;
    )
  );
  ()

let rec encode_app_response_validations (v:app_response_validations) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_app_response_validation x) encoder;
  ) v.validations;
  ()

let rec encode_app_response (v:app_response) encoder = 
  match v with
  | Committed_txs x -> (
    Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_app_response_validations x) encoder;
  )

let rec pp_app_request_txs fmt (v:app_request_txs) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "txs" (Pbrt.Pp.pp_list Raft_com_pb.pp_tx) fmt v.txs;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_app_request fmt (v:app_request) =
  match v with
  | Commit_txs x -> Format.fprintf fmt "@[Commit_txs(%a)@]" pp_app_request_txs x

let rec pp_app_response_validation_failure fmt (v:app_response_validation_failure) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "error_message" Pbrt.Pp.pp_string fmt v.error_message;
    Pbrt.Pp.pp_record_field "error_code" Pbrt.Pp.pp_int fmt v.error_code;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_app_response_validation_result fmt (v:app_response_validation_result) =
  match v with
  | Validation_success  -> Format.fprintf fmt "Validation_success"
  | Validation_failure x -> Format.fprintf fmt "@[Validation_failure(%a)@]" pp_app_response_validation_failure x

and pp_app_response_validation fmt (v:app_response_validation) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "tx_id" Pbrt.Pp.pp_string fmt v.tx_id;
    Pbrt.Pp.pp_record_field "result" pp_app_response_validation_result fmt v.result;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_app_response_validations fmt (v:app_response_validations) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "validations" (Pbrt.Pp.pp_list pp_app_response_validation) fmt v.validations;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_app_response fmt (v:app_response) =
  match v with
  | Committed_txs x -> Format.fprintf fmt "@[Committed_txs(%a)@]" pp_app_response_validations x
