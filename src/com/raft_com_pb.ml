[@@@ocaml.warning "-27-30-39"]

type client_log_entry = {
  client_log_id : string;
  client_log_data : bytes;
}

and client_log_entry_mutable = {
  mutable client_log_id : string;
  mutable client_log_data : bytes;
}

type client_request =
  | Add_log_entry of client_log_entry

type client_response_result = {
  client_log_id : string;
  client_log_result_data : bytes option;
}

and client_response_result_mutable = {
  mutable client_log_id : string;
  mutable client_log_result_data : bytes option;
}

type client_response_not_aleader = {
  leader_id : int option;
}

and client_response_not_aleader_mutable = {
  mutable leader_id : int option;
}

type client_response =
  | Add_log_result of client_response_result
  | Add_log_not_a_leader of client_response_not_aleader

type app_request_add_log_entries = {
  log_entries : Raft_pb.log_entry list;
}

and app_request_add_log_entries_mutable = {
  mutable log_entries : Raft_pb.log_entry list;
}

type app_request =
  | Add_log_entries of app_request_add_log_entries
  | Init

type app_response_result = {
  index : int;
  id : string;
  result_data : bytes option;
}

and app_response_result_mutable = {
  mutable index : int;
  mutable id : string;
  mutable result_data : bytes option;
}

type app_response_results = {
  results : app_response_result list;
  last_log_index : int;
}

and app_response_results_mutable = {
  mutable results : app_response_result list;
  mutable last_log_index : int;
}

type app_response =
  | Add_log_results of app_response_results

let rec default_client_log_entry 
  ?client_log_id:((client_log_id:string) = "")
  ?client_log_data:((client_log_data:bytes) = Bytes.create 0)
  () : client_log_entry  = {
  client_log_id;
  client_log_data;
}

and default_client_log_entry_mutable () : client_log_entry_mutable = {
  client_log_id = "";
  client_log_data = Bytes.create 0;
}

let rec default_client_request () : client_request = Add_log_entry (default_client_log_entry ())

let rec default_client_response_result 
  ?client_log_id:((client_log_id:string) = "")
  ?client_log_result_data:((client_log_result_data:bytes option) = None)
  () : client_response_result  = {
  client_log_id;
  client_log_result_data;
}

and default_client_response_result_mutable () : client_response_result_mutable = {
  client_log_id = "";
  client_log_result_data = None;
}

let rec default_client_response_not_aleader 
  ?leader_id:((leader_id:int option) = None)
  () : client_response_not_aleader  = {
  leader_id;
}

and default_client_response_not_aleader_mutable () : client_response_not_aleader_mutable = {
  leader_id = None;
}

let rec default_client_response () : client_response = Add_log_result (default_client_response_result ())

let rec default_app_request_add_log_entries 
  ?log_entries:((log_entries:Raft_pb.log_entry list) = [])
  () : app_request_add_log_entries  = {
  log_entries;
}

and default_app_request_add_log_entries_mutable () : app_request_add_log_entries_mutable = {
  log_entries = [];
}

let rec default_app_request () : app_request = Add_log_entries (default_app_request_add_log_entries ())

let rec default_app_response_result 
  ?index:((index:int) = 0)
  ?id:((id:string) = "")
  ?result_data:((result_data:bytes option) = None)
  () : app_response_result  = {
  index;
  id;
  result_data;
}

and default_app_response_result_mutable () : app_response_result_mutable = {
  index = 0;
  id = "";
  result_data = None;
}

let rec default_app_response_results 
  ?results:((results:app_response_result list) = [])
  ?last_log_index:((last_log_index:int) = 0)
  () : app_response_results  = {
  results;
  last_log_index;
}

and default_app_response_results_mutable () : app_response_results_mutable = {
  results = [];
  last_log_index = 0;
}

let rec default_app_response () : app_response = Add_log_results (default_app_response_results ())

let rec decode_client_log_entry d =
  let v = default_client_log_entry_mutable () in
  let client_log_data_is_set = ref false in
  let client_log_id_is_set = ref false in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Bytes) -> (
      v.client_log_id <- Pbrt.Decoder.string d; client_log_id_is_set := true;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(client_log_entry), field(1)", pk))
    )
    | Some (2, Pbrt.Bytes) -> (
      v.client_log_data <- Pbrt.Decoder.bytes d; client_log_data_is_set := true;
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(client_log_entry), field(2)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  begin if not !client_log_data_is_set then raise Protobuf.Decoder.(Failure (Missing_field "client_log_data")) end;
  begin if not !client_log_id_is_set then raise Protobuf.Decoder.(Failure (Missing_field "client_log_id")) end;
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

let rec decode_client_response_result d =
  let v = default_client_response_result_mutable () in
  let client_log_id_is_set = ref false in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Bytes) -> (
      v.client_log_id <- Pbrt.Decoder.string d; client_log_id_is_set := true;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(client_response_result), field(1)", pk))
    )
    | Some (2, Pbrt.Bytes) -> (
      v.client_log_result_data <- Some (Pbrt.Decoder.bytes d);
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(client_response_result), field(2)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  begin if not !client_log_id_is_set then raise Protobuf.Decoder.(Failure (Missing_field "client_log_id")) end;
  let v:client_response_result = Obj.magic v in
  v

let rec decode_client_response_not_aleader d =
  let v = default_client_response_not_aleader_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Varint) -> (
      v.leader_id <- Some (Pbrt.Decoder.int_as_varint d);
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(client_response_not_aleader), field(1)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:client_response_not_aleader = Obj.magic v in
  v

let rec decode_client_response d = 
  let rec loop () = 
    let ret:client_response = match Pbrt.Decoder.key d with
      | None -> failwith "None of the known key is found"
      | Some (1, _) -> Add_log_result (decode_client_response_result (Pbrt.Decoder.nested d))
      | Some (2, _) -> Add_log_not_a_leader (decode_client_response_not_aleader (Pbrt.Decoder.nested d))
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
      | Some (4, _) -> (Pbrt.Decoder.empty_nested d ; Init)
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

let rec decode_app_response_result d =
  let v = default_app_response_result_mutable () in
  let id_is_set = ref false in
  let index_is_set = ref false in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Varint) -> (
      v.index <- Pbrt.Decoder.int_as_varint d; index_is_set := true;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(app_response_result), field(1)", pk))
    )
    | Some (2, Pbrt.Bytes) -> (
      v.id <- Pbrt.Decoder.string d; id_is_set := true;
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(app_response_result), field(2)", pk))
    )
    | Some (3, Pbrt.Bytes) -> (
      v.result_data <- Some (Pbrt.Decoder.bytes d);
      loop ()
    )
    | Some (3, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(app_response_result), field(3)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  begin if not !id_is_set then raise Protobuf.Decoder.(Failure (Missing_field "id")) end;
  begin if not !index_is_set then raise Protobuf.Decoder.(Failure (Missing_field "index")) end;
  let v:app_response_result = Obj.magic v in
  v

let rec decode_app_response_results d =
  let v = default_app_response_results_mutable () in
  let last_log_index_is_set = ref false in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
      v.results <- List.rev v.results;
    )
    | Some (1, Pbrt.Bytes) -> (
      v.results <- (decode_app_response_result (Pbrt.Decoder.nested d)) :: v.results;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(app_response_results), field(1)", pk))
    )
    | Some (2, Pbrt.Varint) -> (
      v.last_log_index <- Pbrt.Decoder.int_as_varint d; last_log_index_is_set := true;
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(app_response_results), field(2)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  begin if not !last_log_index_is_set then raise Protobuf.Decoder.(Failure (Missing_field "last_log_index")) end;
  let v:app_response_results = Obj.magic v in
  v

let rec decode_app_response d = 
  let rec loop () = 
    let ret:app_response = match Pbrt.Decoder.key d with
      | None -> failwith "None of the known key is found"
      | Some (4, _) -> Add_log_results (decode_app_response_results (Pbrt.Decoder.nested d))
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
  Pbrt.Encoder.string v.client_log_id encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.bytes v.client_log_data encoder;
  ()

let rec encode_client_request (v:client_request) encoder = 
  match v with
  | Add_log_entry x -> (
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_client_log_entry x) encoder;
  )

let rec encode_client_response_result (v:client_response_result) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.client_log_id encoder;
  (
    match v.client_log_result_data with 
    | Some x -> (
      Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
      Pbrt.Encoder.bytes x encoder;
    )
    | None -> ();
  );
  ()

let rec encode_client_response_not_aleader (v:client_response_not_aleader) encoder = 
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
  | Add_log_result x -> (
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_client_response_result x) encoder;
  )
  | Add_log_not_a_leader x -> (
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_client_response_not_aleader x) encoder;
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
  | Init -> (
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  )

let rec encode_app_response_result (v:app_response_result) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.index encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.id encoder;
  (
    match v.result_data with 
    | Some x -> (
      Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
      Pbrt.Encoder.bytes x encoder;
    )
    | None -> ();
  );
  ()

let rec encode_app_response_results (v:app_response_results) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_app_response_result x) encoder;
  ) v.results;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.last_log_index encoder;
  ()

let rec encode_app_response (v:app_response) encoder = 
  match v with
  | Add_log_results x -> (
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_app_response_results x) encoder;
  )

let rec pp_client_log_entry fmt (v:client_log_entry) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "client_log_id" Pbrt.Pp.pp_string fmt v.client_log_id;
    Pbrt.Pp.pp_record_field "client_log_data" Pbrt.Pp.pp_bytes fmt v.client_log_data;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_client_request fmt (v:client_request) =
  match v with
  | Add_log_entry x -> Format.fprintf fmt "@[Add_log_entry(%a)@]" pp_client_log_entry x

let rec pp_client_response_result fmt (v:client_response_result) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "client_log_id" Pbrt.Pp.pp_string fmt v.client_log_id;
    Pbrt.Pp.pp_record_field "client_log_result_data" (Pbrt.Pp.pp_option Pbrt.Pp.pp_bytes) fmt v.client_log_result_data;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_client_response_not_aleader fmt (v:client_response_not_aleader) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "leader_id" (Pbrt.Pp.pp_option Pbrt.Pp.pp_int) fmt v.leader_id;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_client_response fmt (v:client_response) =
  match v with
  | Add_log_result x -> Format.fprintf fmt "@[Add_log_result(%a)@]" pp_client_response_result x
  | Add_log_not_a_leader x -> Format.fprintf fmt "@[Add_log_not_a_leader(%a)@]" pp_client_response_not_aleader x

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
  | Init  -> Format.fprintf fmt "Init"

let rec pp_app_response_result fmt (v:app_response_result) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "index" Pbrt.Pp.pp_int fmt v.index;
    Pbrt.Pp.pp_record_field "id" Pbrt.Pp.pp_string fmt v.id;
    Pbrt.Pp.pp_record_field "result_data" (Pbrt.Pp.pp_option Pbrt.Pp.pp_bytes) fmt v.result_data;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_app_response_results fmt (v:app_response_results) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "results" (Pbrt.Pp.pp_list pp_app_response_result) fmt v.results;
    Pbrt.Pp.pp_record_field "last_log_index" Pbrt.Pp.pp_int fmt v.last_log_index;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_app_response fmt (v:app_response) =
  match v with
  | Add_log_results x -> Format.fprintf fmt "@[Add_log_results(%a)@]" pp_app_response_results x
