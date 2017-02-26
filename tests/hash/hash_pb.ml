[@@@ocaml.warning "-27-30-39"]

type app_data = {
  random_data : string;
}

and app_data_mutable = {
  mutable random_data : string;
}

type app_result = {
  log_index : int;
  hash : string;
}

and app_result_mutable = {
  mutable log_index : int;
  mutable hash : string;
}

let rec default_app_data 
  ?random_data:((random_data:string) = "")
  () : app_data  = {
  random_data;
}

and default_app_data_mutable () : app_data_mutable = {
  random_data = "";
}

let rec default_app_result 
  ?log_index:((log_index:int) = 0)
  ?hash:((hash:string) = "")
  () : app_result  = {
  log_index;
  hash;
}

and default_app_result_mutable () : app_result_mutable = {
  log_index = 0;
  hash = "";
}

let rec decode_app_data d =
  let v = default_app_data_mutable () in
  let random_data_is_set = ref false in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Bytes) -> (
      v.random_data <- Pbrt.Decoder.string d; random_data_is_set := true;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(app_data), field(1)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  begin if not !random_data_is_set then raise Protobuf.Decoder.(Failure (Missing_field "random_data")) end;
  let v:app_data = Obj.magic v in
  v

let rec decode_app_result d =
  let v = default_app_result_mutable () in
  let hash_is_set = ref false in
  let log_index_is_set = ref false in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Varint) -> (
      v.log_index <- Pbrt.Decoder.int_as_varint d; log_index_is_set := true;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(app_result), field(1)", pk))
    )
    | Some (2, Pbrt.Bytes) -> (
      v.hash <- Pbrt.Decoder.string d; hash_is_set := true;
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(app_result), field(2)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  begin if not !hash_is_set then raise Protobuf.Decoder.(Failure (Missing_field "hash")) end;
  begin if not !log_index_is_set then raise Protobuf.Decoder.(Failure (Missing_field "log_index")) end;
  let v:app_result = Obj.magic v in
  v

let rec encode_app_data (v:app_data) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.random_data encoder;
  ()

let rec encode_app_result (v:app_result) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.log_index encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.hash encoder;
  ()

let rec pp_app_data fmt (v:app_data) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "random_data" Pbrt.Pp.pp_string fmt v.random_data;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_app_result fmt (v:app_result) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "log_index" Pbrt.Pp.pp_int fmt v.log_index;
    Pbrt.Pp.pp_record_field "hash" Pbrt.Pp.pp_string fmt v.hash;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()
