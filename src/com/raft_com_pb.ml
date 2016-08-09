[@@@ocaml.warning "-27-30-39"]

type tx = {
  tx_id : string;
  tx_data : bytes;
}

and tx_mutable = {
  mutable tx_id : string;
  mutable tx_data : bytes;
}

type server_ipc_configuration = {
  raft_id : int;
  inet4_address : string;
  raft_port : int;
  client_port : int;
  app_server_port : int;
}

and server_ipc_configuration_mutable = {
  mutable raft_id : int;
  mutable inet4_address : string;
  mutable raft_port : int;
  mutable client_port : int;
  mutable app_server_port : int;
}

type disk_backup_configuration = {
  compaction_period : float;
  log_record_directory : string;
  compaction_directory : string;
}

and disk_backup_configuration_mutable = {
  mutable compaction_period : float;
  mutable log_record_directory : string;
  mutable compaction_directory : string;
}

type configuration = {
  raft_configuration : Raft_pb.configuration;
  servers_ipc_configuration : server_ipc_configuration list;
  disk_backup : disk_backup_configuration;
}

and configuration_mutable = {
  mutable raft_configuration : Raft_pb.configuration;
  mutable servers_ipc_configuration : server_ipc_configuration list;
  mutable disk_backup : disk_backup_configuration;
}

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

let rec default_server_ipc_configuration 
  ?raft_id:((raft_id:int) = 0)
  ?inet4_address:((inet4_address:string) = "")
  ?raft_port:((raft_port:int) = 0)
  ?client_port:((client_port:int) = 0)
  ?app_server_port:((app_server_port:int) = 0)
  () : server_ipc_configuration  = {
  raft_id;
  inet4_address;
  raft_port;
  client_port;
  app_server_port;
}

and default_server_ipc_configuration_mutable () : server_ipc_configuration_mutable = {
  raft_id = 0;
  inet4_address = "";
  raft_port = 0;
  client_port = 0;
  app_server_port = 0;
}

let rec default_disk_backup_configuration 
  ?compaction_period:((compaction_period:float) = 0.)
  ?log_record_directory:((log_record_directory:string) = "")
  ?compaction_directory:((compaction_directory:string) = "")
  () : disk_backup_configuration  = {
  compaction_period;
  log_record_directory;
  compaction_directory;
}

and default_disk_backup_configuration_mutable () : disk_backup_configuration_mutable = {
  compaction_period = 0.;
  log_record_directory = "";
  compaction_directory = "";
}

let rec default_configuration 
  ?raft_configuration:((raft_configuration:Raft_pb.configuration) = Raft_pb.default_configuration ())
  ?servers_ipc_configuration:((servers_ipc_configuration:server_ipc_configuration list) = [])
  ?disk_backup:((disk_backup:disk_backup_configuration) = default_disk_backup_configuration ())
  () : configuration  = {
  raft_configuration;
  servers_ipc_configuration;
  disk_backup;
}

and default_configuration_mutable () : configuration_mutable = {
  raft_configuration = Raft_pb.default_configuration ();
  servers_ipc_configuration = [];
  disk_backup = default_disk_backup_configuration ();
}

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
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:tx = Obj.magic v in
  v

let rec decode_server_ipc_configuration d =
  let v = default_server_ipc_configuration_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Varint) -> (
      v.raft_id <- Pbrt.Decoder.int_as_varint d;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(server_ipc_configuration), field(1)", pk))
    )
    | Some (2, Pbrt.Bytes) -> (
      v.inet4_address <- Pbrt.Decoder.string d;
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(server_ipc_configuration), field(2)", pk))
    )
    | Some (3, Pbrt.Varint) -> (
      v.raft_port <- Pbrt.Decoder.int_as_varint d;
      loop ()
    )
    | Some (3, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(server_ipc_configuration), field(3)", pk))
    )
    | Some (4, Pbrt.Varint) -> (
      v.client_port <- Pbrt.Decoder.int_as_varint d;
      loop ()
    )
    | Some (4, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(server_ipc_configuration), field(4)", pk))
    )
    | Some (5, Pbrt.Varint) -> (
      v.app_server_port <- Pbrt.Decoder.int_as_varint d;
      loop ()
    )
    | Some (5, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(server_ipc_configuration), field(5)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:server_ipc_configuration = Obj.magic v in
  v

let rec decode_disk_backup_configuration d =
  let v = default_disk_backup_configuration_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Bits64) -> (
      v.compaction_period <- Pbrt.Decoder.float_as_bits64 d;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(disk_backup_configuration), field(1)", pk))
    )
    | Some (2, Pbrt.Bytes) -> (
      v.log_record_directory <- Pbrt.Decoder.string d;
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(disk_backup_configuration), field(2)", pk))
    )
    | Some (3, Pbrt.Bytes) -> (
      v.compaction_directory <- Pbrt.Decoder.string d;
      loop ()
    )
    | Some (3, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(disk_backup_configuration), field(3)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:disk_backup_configuration = Obj.magic v in
  v

let rec decode_configuration d =
  let v = default_configuration_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
      v.servers_ipc_configuration <- List.rev v.servers_ipc_configuration;
    )
    | Some (1, Pbrt.Bytes) -> (
      v.raft_configuration <- Raft_pb.decode_configuration (Pbrt.Decoder.nested d);
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(configuration), field(1)", pk))
    )
    | Some (2, Pbrt.Bytes) -> (
      v.servers_ipc_configuration <- (decode_server_ipc_configuration (Pbrt.Decoder.nested d)) :: v.servers_ipc_configuration;
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(configuration), field(2)", pk))
    )
    | Some (3, Pbrt.Bytes) -> (
      v.disk_backup <- decode_disk_backup_configuration (Pbrt.Decoder.nested d);
      loop ()
    )
    | Some (3, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(configuration), field(3)", pk))
    )
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:configuration = Obj.magic v in
  v

let rec encode_tx (v:tx) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.tx_id encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.bytes v.tx_data encoder;
  ()

let rec encode_server_ipc_configuration (v:server_ipc_configuration) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.raft_id encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.inet4_address encoder;
  Pbrt.Encoder.key (3, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.raft_port encoder;
  Pbrt.Encoder.key (4, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.client_port encoder;
  Pbrt.Encoder.key (5, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.app_server_port encoder;
  ()

let rec encode_disk_backup_configuration (v:disk_backup_configuration) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bits64) encoder; 
  Pbrt.Encoder.float_as_bits64 v.compaction_period encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.log_record_directory encoder;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.compaction_directory encoder;
  ()

let rec encode_configuration (v:configuration) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (Raft_pb.encode_configuration v.raft_configuration) encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_server_ipc_configuration x) encoder;
  ) v.servers_ipc_configuration;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_disk_backup_configuration v.disk_backup) encoder;
  ()

let rec pp_tx fmt (v:tx) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "tx_id" Pbrt.Pp.pp_string fmt v.tx_id;
    Pbrt.Pp.pp_record_field "tx_data" Pbrt.Pp.pp_bytes fmt v.tx_data;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_server_ipc_configuration fmt (v:server_ipc_configuration) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "raft_id" Pbrt.Pp.pp_int fmt v.raft_id;
    Pbrt.Pp.pp_record_field "inet4_address" Pbrt.Pp.pp_string fmt v.inet4_address;
    Pbrt.Pp.pp_record_field "raft_port" Pbrt.Pp.pp_int fmt v.raft_port;
    Pbrt.Pp.pp_record_field "client_port" Pbrt.Pp.pp_int fmt v.client_port;
    Pbrt.Pp.pp_record_field "app_server_port" Pbrt.Pp.pp_int fmt v.app_server_port;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_disk_backup_configuration fmt (v:disk_backup_configuration) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "compaction_period" Pbrt.Pp.pp_float fmt v.compaction_period;
    Pbrt.Pp.pp_record_field "log_record_directory" Pbrt.Pp.pp_string fmt v.log_record_directory;
    Pbrt.Pp.pp_record_field "compaction_directory" Pbrt.Pp.pp_string fmt v.compaction_directory;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_configuration fmt (v:configuration) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "raft_configuration" Raft_pb.pp_configuration fmt v.raft_configuration;
    Pbrt.Pp.pp_record_field "servers_ipc_configuration" (Pbrt.Pp.pp_list pp_server_ipc_configuration) fmt v.servers_ipc_configuration;
    Pbrt.Pp.pp_record_field "disk_backup" pp_disk_backup_configuration fmt v.disk_backup;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()
