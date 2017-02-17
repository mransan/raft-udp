[@@@ocaml.warning "-27-30-39"]

type log_interval_compacted = {
  record_id : string;
}

and log_interval_compacted_mutable = {
  mutable record_id : string;
}

type log_interval_expanded = {
  entries : Raft_pb.log_entry list;
}

and log_interval_expanded_mutable = {
  mutable entries : Raft_pb.log_entry list;
}

type log_interval_rev_log_entries =
  | Compacted of log_interval_compacted
  | Expanded of log_interval_expanded

and log_interval = {
  prev_index : int;
  prev_term : int;
  last_index : int;
  rev_log_entries : log_interval_rev_log_entries;
}

and log_interval_mutable = {
  mutable prev_index : int;
  mutable prev_term : int;
  mutable last_index : int;
  mutable rev_log_entries : log_interval_rev_log_entries;
}

let rec default_log_interval_compacted 
  ?record_id:((record_id:string) = "")
  () : log_interval_compacted  = {
  record_id;
}

and default_log_interval_compacted_mutable () : log_interval_compacted_mutable = {
  record_id = "";
}

let rec default_log_interval_expanded 
  ?entries:((entries:Raft_pb.log_entry list) = [])
  () : log_interval_expanded  = {
  entries;
}

and default_log_interval_expanded_mutable () : log_interval_expanded_mutable = {
  entries = [];
}

let rec default_log_interval_rev_log_entries () : log_interval_rev_log_entries = Compacted (default_log_interval_compacted ())

and default_log_interval 
  ?prev_index:((prev_index:int) = 0)
  ?prev_term:((prev_term:int) = 0)
  ?last_index:((last_index:int) = 0)
  ?rev_log_entries:((rev_log_entries:log_interval_rev_log_entries) = Compacted (default_log_interval_compacted ()))
  () : log_interval  = {
  prev_index;
  prev_term;
  last_index;
  rev_log_entries;
}

and default_log_interval_mutable () : log_interval_mutable = {
  prev_index = 0;
  prev_term = 0;
  last_index = 0;
  rev_log_entries = Compacted (default_log_interval_compacted ());
}

let rec decode_log_interval_compacted d =
  let v = default_log_interval_compacted_mutable () in
  let record_id_is_set = ref false in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Bytes) -> (
      v.record_id <- Pbrt.Decoder.string d; record_id_is_set := true;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(log_interval_compacted), field(1)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  begin if not !record_id_is_set then raise Protobuf.Decoder.(Failure (Missing_field "record_id")) end;
  let v:log_interval_compacted = Obj.magic v in
  v

let rec decode_log_interval_expanded d =
  let v = default_log_interval_expanded_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
      v.entries <- List.rev v.entries;
    )
    | Some (1, Pbrt.Bytes) -> (
      v.entries <- (Raft_pb.decode_log_entry (Pbrt.Decoder.nested d)) :: v.entries;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(log_interval_expanded), field(1)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:log_interval_expanded = Obj.magic v in
  v

let rec decode_log_interval_rev_log_entries d = 
  let rec loop () = 
    let ret:log_interval_rev_log_entries = match Pbrt.Decoder.key d with
      | None -> failwith "None of the known key is found"
      | Some (4, _) -> Compacted (decode_log_interval_compacted (Pbrt.Decoder.nested d))
      | Some (5, _) -> Expanded (decode_log_interval_expanded (Pbrt.Decoder.nested d))
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

and decode_log_interval d =
  let v = default_log_interval_mutable () in
  let last_index_is_set = ref false in
  let prev_term_is_set = ref false in
  let prev_index_is_set = ref false in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Varint) -> (
      v.prev_index <- Pbrt.Decoder.int_as_varint d; prev_index_is_set := true;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(log_interval), field(1)", pk))
    )
    | Some (2, Pbrt.Varint) -> (
      v.prev_term <- Pbrt.Decoder.int_as_varint d; prev_term_is_set := true;
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(log_interval), field(2)", pk))
    )
    | Some (3, Pbrt.Varint) -> (
      v.last_index <- Pbrt.Decoder.int_as_varint d; last_index_is_set := true;
      loop ()
    )
    | Some (3, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(log_interval), field(3)", pk))
    )
    | Some (4, Pbrt.Bytes) -> (
      v.rev_log_entries <- Compacted (decode_log_interval_compacted (Pbrt.Decoder.nested d));
      loop ()
    )
    | Some (4, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(log_interval), field(4)", pk))
    )
    | Some (5, Pbrt.Bytes) -> (
      v.rev_log_entries <- Expanded (decode_log_interval_expanded (Pbrt.Decoder.nested d));
      loop ()
    )
    | Some (5, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(log_interval), field(5)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  begin if not !last_index_is_set then raise Protobuf.Decoder.(Failure (Missing_field "last_index")) end;
  begin if not !prev_term_is_set then raise Protobuf.Decoder.(Failure (Missing_field "prev_term")) end;
  begin if not !prev_index_is_set then raise Protobuf.Decoder.(Failure (Missing_field "prev_index")) end;
  let v:log_interval = Obj.magic v in
  v

let rec encode_log_interval_compacted (v:log_interval_compacted) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.record_id encoder;
  ()

let rec encode_log_interval_expanded (v:log_interval_expanded) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (Raft_pb.encode_log_entry x) encoder;
  ) v.entries;
  ()

let rec encode_log_interval_rev_log_entries (v:log_interval_rev_log_entries) encoder = 
  match v with
  | Compacted x -> (
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_log_interval_compacted x) encoder;
  )
  | Expanded x -> (
    Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_log_interval_expanded x) encoder;
  )

and encode_log_interval (v:log_interval) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.prev_index encoder;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.prev_term encoder;
  Pbrt.Encoder.key (3, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.last_index encoder;
  (
    match v.rev_log_entries with
    | Compacted x -> (
      Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
      Pbrt.Encoder.nested (encode_log_interval_compacted x) encoder;
    )
    | Expanded x -> (
      Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
      Pbrt.Encoder.nested (encode_log_interval_expanded x) encoder;
    )
  );
  ()

let rec pp_log_interval_compacted fmt (v:log_interval_compacted) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "record_id" Pbrt.Pp.pp_string fmt v.record_id;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_log_interval_expanded fmt (v:log_interval_expanded) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "entries" (Pbrt.Pp.pp_list Raft_pb.pp_log_entry) fmt v.entries;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_log_interval_rev_log_entries fmt (v:log_interval_rev_log_entries) =
  match v with
  | Compacted x -> Format.fprintf fmt "@[Compacted(%a)@]" pp_log_interval_compacted x
  | Expanded x -> Format.fprintf fmt "@[Expanded(%a)@]" pp_log_interval_expanded x

and pp_log_interval fmt (v:log_interval) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "prev_index" Pbrt.Pp.pp_int fmt v.prev_index;
    Pbrt.Pp.pp_record_field "prev_term" Pbrt.Pp.pp_int fmt v.prev_term;
    Pbrt.Pp.pp_record_field "last_index" Pbrt.Pp.pp_int fmt v.last_index;
    Pbrt.Pp.pp_record_field "rev_log_entries" pp_log_interval_rev_log_entries fmt v.rev_log_entries;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()
