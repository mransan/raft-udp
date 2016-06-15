[@@@ocaml.warning "-30"]

type tx = {
  tx_id : string;
  tx_data : bytes;
}

and tx_mutable = {
  mutable tx_id : string;
  mutable tx_data : bytes;
}

type client_request =
  | Add_tx of tx

type client_response_add_log_not_aleader = {
  leader_id : int option;
}

and client_response_add_log_not_aleader_mutable = {
  mutable leader_id : int option;
}

type client_response =
  | Add_log_success
  | Add_log_validation_failure
  | Add_log_not_a_leader of client_response_add_log_not_aleader

type app_request_validate_txs = {
  txs : tx list;
}

and app_request_validate_txs_mutable = {
  mutable txs : tx list;
}

type app_request =
  | Validate_txs of app_request_validate_txs
  | Commit_tx of tx

type app_response_validation_failure = {
  error_message : string;
  error_code : int;
}

and app_response_validation_failure_mutable = {
  mutable error_message : string;
  mutable error_code : int;
}

type app_response_validation_result =
  | Success
  | Failure of app_response_validation_failure

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

type app_response_commit_tx_ack = {
  tx_id : string;
}

and app_response_commit_tx_ack_mutable = {
  mutable tx_id : string;
}

type app_response =
  | Validations of app_response_validations
  | Commit_tx_ack of app_response_commit_tx_ack

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

let rec default_client_request () : client_request = Add_tx (default_tx ())

let rec default_client_response_add_log_not_aleader 
  ?leader_id:((leader_id:int option) = None)
  () : client_response_add_log_not_aleader  = {
  leader_id;
}

and default_client_response_add_log_not_aleader_mutable () : client_response_add_log_not_aleader_mutable = {
  leader_id = None;
}

let rec default_client_response (): client_response = Add_log_success

let rec default_app_request_validate_txs 
  ?txs:((txs:tx list) = [])
  () : app_request_validate_txs  = {
  txs;
}

and default_app_request_validate_txs_mutable () : app_request_validate_txs_mutable = {
  txs = [];
}

let rec default_app_request () : app_request = Validate_txs (default_app_request_validate_txs ())

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

let rec default_app_response_validation_result (): app_response_validation_result = Success

and default_app_response_validation 
  ?tx_id:((tx_id:string) = "")
  ?result:((result:app_response_validation_result) = Success)
  () : app_response_validation  = {
  tx_id;
  result;
}

and default_app_response_validation_mutable () : app_response_validation_mutable = {
  tx_id = "";
  result = Success;
}

let rec default_app_response_validations 
  ?validations:((validations:app_response_validation list) = [])
  () : app_response_validations  = {
  validations;
}

and default_app_response_validations_mutable () : app_response_validations_mutable = {
  validations = [];
}

let rec default_app_response_commit_tx_ack 
  ?tx_id:((tx_id:string) = "")
  () : app_response_commit_tx_ack  = {
  tx_id;
}

and default_app_response_commit_tx_ack_mutable () : app_response_commit_tx_ack_mutable = {
  tx_id = "";
}

let rec default_app_response () : app_response = Validations (default_app_response_validations ())

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
    | Some (n, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:tx = Obj.magic v in
  v

let rec decode_client_request d = 
  let rec loop () = 
    let ret:client_request = match Pbrt.Decoder.key d with
      | None -> failwith "None of the known key is found"
      | Some (1, _) -> Add_tx (decode_tx (Pbrt.Decoder.nested d))
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

let rec decode_client_response_add_log_not_aleader d =
  let v = default_client_response_add_log_not_aleader_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Varint) -> (
      v.leader_id <- Some (Pbrt.Decoder.int_as_varint d);
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(client_response_add_log_not_aleader), field(1)", pk))
    )
    | Some (n, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:client_response_add_log_not_aleader = Obj.magic v in
  v

let rec decode_client_response d = 
  let rec loop () = 
    let ret:client_response = match Pbrt.Decoder.key d with
      | None -> failwith "None of the known key is found"
      | Some (1, _) -> (Pbrt.Decoder.empty_nested d ; Add_log_success)
      | Some (2, _) -> (Pbrt.Decoder.empty_nested d ; Add_log_validation_failure)
      | Some (3, _) -> Add_log_not_a_leader (decode_client_response_add_log_not_aleader (Pbrt.Decoder.nested d))
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

let rec decode_app_request_validate_txs d =
  let v = default_app_request_validate_txs_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
      v.txs <- List.rev v.txs;
    )
    | Some (1, Pbrt.Bytes) -> (
      v.txs <- (decode_tx (Pbrt.Decoder.nested d)) :: v.txs;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(app_request_validate_txs), field(1)", pk))
    )
    | Some (n, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:app_request_validate_txs = Obj.magic v in
  v

let rec decode_app_request d = 
  let rec loop () = 
    let ret:app_request = match Pbrt.Decoder.key d with
      | None -> failwith "None of the known key is found"
      | Some (3, _) -> Validate_txs (decode_app_request_validate_txs (Pbrt.Decoder.nested d))
      | Some (4, _) -> Commit_tx (decode_tx (Pbrt.Decoder.nested d))
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
    | Some (n, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:app_response_validation_failure = Obj.magic v in
  v

let rec decode_app_response_validation_result d = 
  let rec loop () = 
    let ret:app_response_validation_result = match Pbrt.Decoder.key d with
      | None -> failwith "None of the known key is found"
      | Some (2, _) -> (Pbrt.Decoder.empty_nested d ; Success)
      | Some (3, _) -> Failure (decode_app_response_validation_failure (Pbrt.Decoder.nested d))
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
      v.result <- Success;
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(app_response_validation), field(2)", pk))
    )
    | Some (3, Pbrt.Bytes) -> (
      v.result <- Failure (decode_app_response_validation_failure (Pbrt.Decoder.nested d));
      loop ()
    )
    | Some (3, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(app_response_validation), field(3)", pk))
    )
    | Some (n, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
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
    | Some (n, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:app_response_validations = Obj.magic v in
  v

let rec decode_app_response_commit_tx_ack d =
  let v = default_app_response_commit_tx_ack_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (2, Pbrt.Bytes) -> (
      v.tx_id <- Pbrt.Decoder.string d;
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(app_response_commit_tx_ack), field(2)", pk))
    )
    | Some (n, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:app_response_commit_tx_ack = Obj.magic v in
  v

let rec decode_app_response d = 
  let rec loop () = 
    let ret:app_response = match Pbrt.Decoder.key d with
      | None -> failwith "None of the known key is found"
      | Some (4, _) -> Validations (decode_app_response_validations (Pbrt.Decoder.nested d))
      | Some (5, _) -> Commit_tx_ack (decode_app_response_commit_tx_ack (Pbrt.Decoder.nested d))
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

let rec encode_tx (v:tx) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.tx_id encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.bytes v.tx_data encoder;
  ()

let rec encode_client_request (v:client_request) encoder = 
  match v with
  | Add_tx x -> (
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_tx x) encoder;
  )

let rec encode_client_response_add_log_not_aleader (v:client_response_add_log_not_aleader) encoder = 
  (
    match v.leader_id with 
    | Some x -> (
      Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
      Pbrt.Encoder.int_as_varint x encoder;
    )
    | None -> ();
  );
  ()

let rec encode_client_response (v:client_response) encoder = 
  match v with
  | Add_log_success -> (
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  )
  | Add_log_validation_failure -> (
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  )
  | Add_log_not_a_leader x -> (
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_client_response_add_log_not_aleader x) encoder;
  )

let rec encode_app_request_validate_txs (v:app_request_validate_txs) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_tx x) encoder;
  ) v.txs;
  ()

let rec encode_app_request (v:app_request) encoder = 
  match v with
  | Validate_txs x -> (
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_app_request_validate_txs x) encoder;
  )
  | Commit_tx x -> (
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_tx x) encoder;
  )

let rec encode_app_response_validation_failure (v:app_response_validation_failure) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.error_message encoder;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.error_code encoder;
  ()

let rec encode_app_response_validation_result (v:app_response_validation_result) encoder = 
  match v with
  | Success -> (
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  )
  | Failure x -> (
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_app_response_validation_failure x) encoder;
  )

and encode_app_response_validation (v:app_response_validation) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.tx_id encoder;
  (
    match v.result with
    | Success -> (
      Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
      Pbrt.Encoder.empty_nested encoder
    )
    | Failure x -> (
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

let rec encode_app_response_commit_tx_ack (v:app_response_commit_tx_ack) encoder = 
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.tx_id encoder;
  ()

let rec encode_app_response (v:app_response) encoder = 
  match v with
  | Validations x -> (
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_app_response_validations x) encoder;
  )
  | Commit_tx_ack x -> (
    Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_app_response_commit_tx_ack x) encoder;
  )

let rec pp_tx fmt (v:tx) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "tx_id" Pbrt.Pp.pp_string fmt v.tx_id;
    Pbrt.Pp.pp_record_field "tx_data" Pbrt.Pp.pp_bytes fmt v.tx_data;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_client_request fmt (v:client_request) =
  match v with
  | Add_tx x -> Format.fprintf fmt "@[Add_tx(%a)@]" pp_tx x

let rec pp_client_response_add_log_not_aleader fmt (v:client_response_add_log_not_aleader) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "leader_id" (Pbrt.Pp.pp_option Pbrt.Pp.pp_int) fmt v.leader_id;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_client_response fmt (v:client_response) =
  match v with
  | Add_log_success  -> Format.fprintf fmt "Add_log_success"
  | Add_log_validation_failure  -> Format.fprintf fmt "Add_log_validation_failure"
  | Add_log_not_a_leader x -> Format.fprintf fmt "@[Add_log_not_a_leader(%a)@]" pp_client_response_add_log_not_aleader x

let rec pp_app_request_validate_txs fmt (v:app_request_validate_txs) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "txs" (Pbrt.Pp.pp_list pp_tx) fmt v.txs;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_app_request fmt (v:app_request) =
  match v with
  | Validate_txs x -> Format.fprintf fmt "@[Validate_txs(%a)@]" pp_app_request_validate_txs x
  | Commit_tx x -> Format.fprintf fmt "@[Commit_tx(%a)@]" pp_tx x

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
  | Success  -> Format.fprintf fmt "Success"
  | Failure x -> Format.fprintf fmt "@[Failure(%a)@]" pp_app_response_validation_failure x

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

let rec pp_app_response_commit_tx_ack fmt (v:app_response_commit_tx_ack) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "tx_id" Pbrt.Pp.pp_string fmt v.tx_id;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_app_response fmt (v:app_response) =
  match v with
  | Validations x -> Format.fprintf fmt "@[Validations(%a)@]" pp_app_response_validations x
  | Commit_tx_ack x -> Format.fprintf fmt "@[Commit_tx_ack(%a)@]" pp_app_response_commit_tx_ack x
