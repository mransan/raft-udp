[@@@ocaml.warning "-27-30-39"]

type app_data = {
  increment : int;
  process_id : int;
}

and app_data_mutable = {
  mutable increment : int;
  mutable process_id : int;
}

type app_result = {
  from : int;
  to_ : int option;
}

and app_result_mutable = {
  mutable from : int;
  mutable to_ : int option;
}

let rec default_app_data 
  ?increment:((increment:int) = 0)
  ?process_id:((process_id:int) = 0)
  () : app_data  = {
  increment;
  process_id;
}

and default_app_data_mutable () : app_data_mutable = {
  increment = 0;
  process_id = 0;
}

let rec default_app_result 
  ?from:((from:int) = 0)
  ?to_:((to_:int option) = None)
  () : app_result  = {
  from;
  to_;
}

and default_app_result_mutable () : app_result_mutable = {
  from = 0;
  to_ = None;
}

let rec decode_app_data d =
  let v = default_app_data_mutable () in
  let process_id_is_set = ref false in
  let increment_is_set = ref false in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Varint) -> (
      v.increment <- Pbrt.Decoder.int_as_varint d; increment_is_set := true;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(app_data), field(1)", pk))
    )
    | Some (2, Pbrt.Varint) -> (
      v.process_id <- Pbrt.Decoder.int_as_varint d; process_id_is_set := true;
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(app_data), field(2)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  begin if not !process_id_is_set then raise Protobuf.Decoder.(Failure (Missing_field "process_id")) end;
  begin if not !increment_is_set then raise Protobuf.Decoder.(Failure (Missing_field "increment")) end;
  let v:app_data = Obj.magic v in
  v

let rec decode_app_result d =
  let v = default_app_result_mutable () in
  let from_is_set = ref false in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Varint) -> (
      v.from <- Pbrt.Decoder.int_as_varint d; from_is_set := true;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(app_result), field(1)", pk))
    )
    | Some (2, Pbrt.Varint) -> (
      v.to_ <- Some (Pbrt.Decoder.int_as_varint d);
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(app_result), field(2)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  begin if not !from_is_set then raise Protobuf.Decoder.(Failure (Missing_field "from")) end;
  let v:app_result = Obj.magic v in
  v

let rec encode_app_data (v:app_data) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.increment encoder;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.process_id encoder;
  ()

let rec encode_app_result (v:app_result) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.from encoder;
  (
    match v.to_ with 
    | Some x -> (
      Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
      Pbrt.Encoder.int_as_varint x encoder;
    )
    | None -> ();
  );
  ()

let rec pp_app_data fmt (v:app_data) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "increment" Pbrt.Pp.pp_int fmt v.increment;
    Pbrt.Pp.pp_record_field "process_id" Pbrt.Pp.pp_int fmt v.process_id;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_app_result fmt (v:app_result) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "from" Pbrt.Pp.pp_int fmt v.from;
    Pbrt.Pp.pp_record_field "to_" (Pbrt.Pp.pp_option Pbrt.Pp.pp_int) fmt v.to_;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()
