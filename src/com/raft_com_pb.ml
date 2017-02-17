[@@@ocaml.warning "-27-30-39"]

type client_log_entry = {
  id : string;
  data : bytes;
}

and client_log_entry_mutable = {
  mutable id : string;
  mutable data : bytes;
}

type client_request =
  | Add_log_entry of client_log_entry

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

type app_request_add_log_entries = {
  log_entries : Raft_pb.log_entry list;
}

and app_request_add_log_entries_mutable = {
  mutable log_entries : Raft_pb.log_entry list;
}

type app_request =
  | Add_log_entries of app_request_add_log_entries

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
  id : string;
  result : app_response_validation_result;
}

and app_response_validation_mutable = {
  mutable id : string;
  mutable result : app_response_validation_result;
}

type app_response_validations = {
  validations : app_response_validation list;
}

and app_response_validations_mutable = {
  mutable validations : app_response_validation list;
}

type app_response =
  | Validations of app_response_validations

let rec default_client_log_entry 
  ?id:((id:string) = "")
  ?data:((data:bytes) = Bytes.create 0)
  () : client_log_entry  = {
  id;
  data;
}

and default_client_log_entry_mutable () : client_log_entry_mutable = {
  id = "";
  data = Bytes.create 0;
}

let rec default_client_request () : client_request = Add_log_entry (default_client_log_entry ())

let rec default_client_response_add_log_not_aleader 
  ?leader_id:((leader_id:int option) = None)
  () : client_response_add_log_not_aleader  = {
  leader_id;
}

and default_client_response_add_log_not_aleader_mutable () : client_response_add_log_not_aleader_mutable = {
  leader_id = None;
}

let rec default_client_response (): client_response = Add_log_success

let rec default_app_request_add_log_entries 
  ?log_entries:((log_entries:Raft_pb.log_entry list) = [])
  () : app_request_add_log_entries  = {
  log_entries;
}

and default_app_request_add_log_entries_mutable () : app_request_add_log_entries_mutable = {
  log_entries = [];
}

let rec default_app_request () : app_request = Add_log_entries (default_app_request_add_log_entries ())

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
  ?id:((id:string) = "")
  ?result:((result:app_response_validation_result) = Validation_success)
  () : app_response_validation  = {
  id;
  result;
}

and default_app_response_validation_mutable () : app_response_validation_mutable = {
  id = "";
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

let rec default_app_response () : app_response = Validations (default_app_response_validations ())

let rec decode_client_log_entry d =
  let v = default_client_log_entry_mutable () in
  let data_is_set = ref false in
  let id_is_set = ref false in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Bytes) -> (
      v.id <- Pbrt.Decoder.string d; id_is_set := true;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(client_log_entry), field(1)", pk))
    )
    | Some (2, Pbrt.Bytes) -> (
      v.data <- Pbrt.Decoder.bytes d; data_is_set := true;
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(client_log_entry), field(2)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  begin if not !data_is_set then raise Protobuf.Decoder.(Failure (Missing_field "data")) end;
  begin if not !id_is_set then raise Protobuf.Decoder.(Failure (Missing_field "id")) end;
  let v:client_log_entry = Obj.magic v in
  v

let rec decode_client_request d = 
  let rec loop () = 
    let ret:client_request = match Pbrt.Decoder.key d with
      | None -> failwith "None of the known key is found"
      | Some (1, _) -> Add_log_entry (decode_client_log_entry (Pbrt.Decoder.nested d))
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
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
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

let rec decode_app_request_add_log_entries d =
  let v = default_app_request_add_log_entries_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
      v.log_entries <- List.rev v.log_entries;
    )
    | Some (1, Pbrt.Bytes) -> (
      v.log_entries <- (Raft_pb.decode_log_entry (Pbrt.Decoder.nested d)) :: v.log_entries;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(app_request_add_log_entries), field(1)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:app_request_add_log_entries = Obj.magic v in
  v

let rec decode_app_request d = 
  let rec loop () = 
    let ret:app_request = match Pbrt.Decoder.key d with
      | None -> failwith "None of the known key is found"
      | Some (3, _) -> Add_log_entries (decode_app_request_add_log_entries (Pbrt.Decoder.nested d))
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
  let error_code_is_set = ref false in
  let error_message_is_set = ref false in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Bytes) -> (
      v.error_message <- Pbrt.Decoder.string d; error_message_is_set := true;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(app_response_validation_failure), field(1)", pk))
    )
    | Some (2, Pbrt.Varint) -> (
      v.error_code <- Pbrt.Decoder.int_as_varint d; error_code_is_set := true;
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(app_response_validation_failure), field(2)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  begin if not !error_code_is_set then raise Protobuf.Decoder.(Failure (Missing_field "error_code")) end;
  begin if not !error_message_is_set then raise Protobuf.Decoder.(Failure (Missing_field "error_message")) end;
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
  let id_is_set = ref false in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Bytes) -> (
      v.id <- Pbrt.Decoder.string d; id_is_set := true;
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
  begin if not !id_is_set then raise Protobuf.Decoder.(Failure (Missing_field "id")) end;
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
      | Some (4, _) -> Validations (decode_app_response_validations (Pbrt.Decoder.nested d))
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

let rec encode_client_log_entry (v:client_log_entry) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.id encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.bytes v.data encoder;
  ()

let rec encode_client_request (v:client_request) encoder = 
  match v with
  | Add_log_entry x -> (
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_client_log_entry x) encoder;
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

let rec encode_app_request_add_log_entries (v:app_request_add_log_entries) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (Raft_pb.encode_log_entry x) encoder;
  ) v.log_entries;
  ()

let rec encode_app_request (v:app_request) encoder = 
  match v with
  | Add_log_entries x -> (
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_app_request_add_log_entries x) encoder;
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
  Pbrt.Encoder.string v.id encoder;
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
  | Validations x -> (
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_app_response_validations x) encoder;
  )

let rec pp_client_log_entry fmt (v:client_log_entry) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "id" Pbrt.Pp.pp_string fmt v.id;
    Pbrt.Pp.pp_record_field "data" Pbrt.Pp.pp_bytes fmt v.data;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_client_request fmt (v:client_request) =
  match v with
  | Add_log_entry x -> Format.fprintf fmt "@[Add_log_entry(%a)@]" pp_client_log_entry x

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

let rec pp_app_request_add_log_entries fmt (v:app_request_add_log_entries) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "log_entries" (Pbrt.Pp.pp_list Raft_pb.pp_log_entry) fmt v.log_entries;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_app_request fmt (v:app_request) =
  match v with
  | Add_log_entries x -> Format.fprintf fmt "@[Add_log_entries(%a)@]" pp_app_request_add_log_entries x

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
    Pbrt.Pp.pp_record_field "id" Pbrt.Pp.pp_string fmt v.id;
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
  | Validations x -> Format.fprintf fmt "@[Validations(%a)@]" pp_app_response_validations x
