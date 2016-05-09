[@@@ocaml.warning "-30"]

type server_udp_configuration = {
  raft_id : int;
  inet4_address : string;
  port : int;
}

and server_udp_configuration_mutable = {
  mutable raft_id : int;
  mutable inet4_address : string;
  mutable port : int;
}

type configuration = {
  raft_configuration : Raft_pb.configuration;
  servers_udp_configuration : server_udp_configuration list;
}

and configuration_mutable = {
  mutable raft_configuration : Raft_pb.configuration;
  mutable servers_udp_configuration : server_udp_configuration list;
}

let rec default_server_udp_configuration 
  ?raft_id:((raft_id:int) = 0)
  ?inet4_address:((inet4_address:string) = "")
  ?port:((port:int) = 0)
  () : server_udp_configuration  = {
  raft_id;
  inet4_address;
  port;
}

and default_server_udp_configuration_mutable () : server_udp_configuration_mutable = {
  raft_id = 0;
  inet4_address = "";
  port = 0;
}

let rec default_configuration 
  ?raft_configuration:((raft_configuration:Raft_pb.configuration) = Raft_pb.default_configuration ())
  ?servers_udp_configuration:((servers_udp_configuration:server_udp_configuration list) = [])
  () : configuration  = {
  raft_configuration;
  servers_udp_configuration;
}

and default_configuration_mutable () : configuration_mutable = {
  raft_configuration = Raft_pb.default_configuration ();
  servers_udp_configuration = [];
}

let rec decode_server_udp_configuration d =
  let v = default_server_udp_configuration_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Varint) -> (
      v.raft_id <- Pbrt.Decoder.int_as_varint d;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(server_udp_configuration), field(1)", pk))
    )
    | Some (2, Pbrt.Bytes) -> (
      v.inet4_address <- Pbrt.Decoder.string d;
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(server_udp_configuration), field(2)", pk))
    )
    | Some (3, Pbrt.Varint) -> (
      v.port <- Pbrt.Decoder.int_as_varint d;
      loop ()
    )
    | Some (3, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(server_udp_configuration), field(3)", pk))
    )
    | Some (n, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:server_udp_configuration = Obj.magic v in
  v

let rec decode_configuration d =
  let v = default_configuration_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
      v.servers_udp_configuration <- List.rev v.servers_udp_configuration;
    )
    | Some (1, Pbrt.Bytes) -> (
      v.raft_configuration <- Raft_pb.decode_configuration (Pbrt.Decoder.nested d);
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(configuration), field(1)", pk))
    )
    | Some (2, Pbrt.Bytes) -> (
      v.servers_udp_configuration <- (decode_server_udp_configuration (Pbrt.Decoder.nested d)) :: v.servers_udp_configuration;
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(configuration), field(2)", pk))
    )
    | Some (n, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:configuration = Obj.magic v in
  v

let rec encode_server_udp_configuration (v:server_udp_configuration) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.raft_id encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.inet4_address encoder;
  Pbrt.Encoder.key (3, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.port encoder;
  ()

let rec encode_configuration (v:configuration) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (Raft_pb.encode_configuration v.raft_configuration) encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_server_udp_configuration x) encoder;
  ) v.servers_udp_configuration;
  ()

let rec pp_server_udp_configuration fmt (v:server_udp_configuration) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "raft_id" Pbrt.Pp.pp_int fmt v.raft_id;
    Pbrt.Pp.pp_record_field "inet4_address" Pbrt.Pp.pp_string fmt v.inet4_address;
    Pbrt.Pp.pp_record_field "port" Pbrt.Pp.pp_int fmt v.port;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_configuration fmt (v:configuration) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "raft_configuration" Raft_pb.pp_configuration fmt v.raft_configuration;
    Pbrt.Pp.pp_record_field "servers_udp_configuration" (Pbrt.Pp.pp_list pp_server_udp_configuration) fmt v.servers_udp_configuration;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()
