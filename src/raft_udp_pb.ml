[@@@ocaml.warning "-30"]

type server_udp_configuration = {
  raft_id : int;
  inet4_address : string;
  raft_port : int;
  client_port : int;
}

and server_udp_configuration_mutable = {
  mutable raft_id : int;
  mutable inet4_address : string;
  mutable raft_port : int;
  mutable client_port : int;
}

type configuration = {
  raft_configuration : Raft_pb.configuration;
  servers_udp_configuration : server_udp_configuration list;
  compaction_period : float;
  log_record_directory : string;
  compaction_directory : string;
}

and configuration_mutable = {
  mutable raft_configuration : Raft_pb.configuration;
  mutable servers_udp_configuration : server_udp_configuration list;
  mutable compaction_period : float;
  mutable log_record_directory : string;
  mutable compaction_directory : string;
}

type log_entry = {
  request_id : string;
  data : bytes;
}

and log_entry_mutable = {
  mutable request_id : string;
  mutable data : bytes;
}

type client_request =
  | Add_log of log_entry

type client_response_add_log_not_aleader = {
  leader_id : int option;
}

and client_response_add_log_not_aleader_mutable = {
  mutable leader_id : int option;
}

type client_response =
  | Add_log_success
  | Add_log_replication_failure
  | Add_log_not_a_leader of client_response_add_log_not_aleader

type app_request =
  | Validate_log of log_entry
  | Commit_log of log_entry

type app_response_failure = {
  error_message : string;
  error_code : int;
}

and app_response_failure_mutable = {
  mutable error_message : string;
  mutable error_code : int;
}

type app_response =
  | Success
  | Failure of app_response_failure

let rec default_server_udp_configuration 
  ?raft_id:((raft_id:int) = 0)
  ?inet4_address:((inet4_address:string) = "")
  ?raft_port:((raft_port:int) = 0)
  ?client_port:((client_port:int) = 0)
  () : server_udp_configuration  = {
  raft_id;
  inet4_address;
  raft_port;
  client_port;
}

and default_server_udp_configuration_mutable () : server_udp_configuration_mutable = {
  raft_id = 0;
  inet4_address = "";
  raft_port = 0;
  client_port = 0;
}

let rec default_configuration 
  ?raft_configuration:((raft_configuration:Raft_pb.configuration) = Raft_pb.default_configuration ())
  ?servers_udp_configuration:((servers_udp_configuration:server_udp_configuration list) = [])
  ?compaction_period:((compaction_period:float) = 0.)
  ?log_record_directory:((log_record_directory:string) = "")
  ?compaction_directory:((compaction_directory:string) = "")
  () : configuration  = {
  raft_configuration;
  servers_udp_configuration;
  compaction_period;
  log_record_directory;
  compaction_directory;
}

and default_configuration_mutable () : configuration_mutable = {
  raft_configuration = Raft_pb.default_configuration ();
  servers_udp_configuration = [];
  compaction_period = 0.;
  log_record_directory = "";
  compaction_directory = "";
}

let rec default_log_entry 
  ?request_id:((request_id:string) = "")
  ?data:((data:bytes) = Bytes.create 64)
  () : log_entry  = {
  request_id;
  data;
}

and default_log_entry_mutable () : log_entry_mutable = {
  request_id = "";
  data = Bytes.create 64;
}

let rec default_client_request () : client_request = Add_log (default_log_entry ())

let rec default_client_response_add_log_not_aleader 
  ?leader_id:((leader_id:int option) = None)
  () : client_response_add_log_not_aleader  = {
  leader_id;
}

and default_client_response_add_log_not_aleader_mutable () : client_response_add_log_not_aleader_mutable = {
  leader_id = None;
}

let rec default_client_response (): client_response = Add_log_success

let rec default_app_request () : app_request = Validate_log (default_log_entry ())

let rec default_app_response_failure 
  ?error_message:((error_message:string) = "")
  ?error_code:((error_code:int) = 0)
  () : app_response_failure  = {
  error_message;
  error_code;
}

and default_app_response_failure_mutable () : app_response_failure_mutable = {
  error_message = "";
  error_code = 0;
}

let rec default_app_response (): app_response = Success

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
      v.raft_port <- Pbrt.Decoder.int_as_varint d;
      loop ()
    )
    | Some (3, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(server_udp_configuration), field(3)", pk))
    )
    | Some (4, Pbrt.Varint) -> (
      v.client_port <- Pbrt.Decoder.int_as_varint d;
      loop ()
    )
    | Some (4, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(server_udp_configuration), field(4)", pk))
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
    | Some (3, Pbrt.Bits64) -> (
      v.compaction_period <- Pbrt.Decoder.float_as_bits64 d;
      loop ()
    )
    | Some (3, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(configuration), field(3)", pk))
    )
    | Some (4, Pbrt.Bytes) -> (
      v.log_record_directory <- Pbrt.Decoder.string d;
      loop ()
    )
    | Some (4, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(configuration), field(4)", pk))
    )
    | Some (5, Pbrt.Bytes) -> (
      v.compaction_directory <- Pbrt.Decoder.string d;
      loop ()
    )
    | Some (5, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(configuration), field(5)", pk))
    )
    | Some (n, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:configuration = Obj.magic v in
  v

let rec decode_log_entry d =
  let v = default_log_entry_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Bytes) -> (
      v.request_id <- Pbrt.Decoder.string d;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(log_entry), field(1)", pk))
    )
    | Some (2, Pbrt.Bytes) -> (
      v.data <- Pbrt.Decoder.bytes d;
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(log_entry), field(2)", pk))
    )
    | Some (n, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:log_entry = Obj.magic v in
  v

let rec decode_client_request d = 
  let rec loop () = 
    let ret:client_request = match Pbrt.Decoder.key d with
      | None -> failwith "None of the known key is found"
      | Some (1, _) -> Add_log (decode_log_entry (Pbrt.Decoder.nested d))
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
      | Some (2, _) -> (Pbrt.Decoder.empty_nested d ; Add_log_replication_failure)
      | Some (3, _) -> Add_log_not_a_leader (decode_client_response_add_log_not_aleader (Pbrt.Decoder.nested d))
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

let rec decode_app_request d = 
  let rec loop () = 
    let ret:app_request = match Pbrt.Decoder.key d with
      | None -> failwith "None of the known key is found"
      | Some (1, _) -> Validate_log (decode_log_entry (Pbrt.Decoder.nested d))
      | Some (2, _) -> Commit_log (decode_log_entry (Pbrt.Decoder.nested d))
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

let rec decode_app_response_failure d =
  let v = default_app_response_failure_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Bytes) -> (
      v.error_message <- Pbrt.Decoder.string d;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(app_response_failure), field(1)", pk))
    )
    | Some (2, Pbrt.Varint) -> (
      v.error_code <- Pbrt.Decoder.int_as_varint d;
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(app_response_failure), field(2)", pk))
    )
    | Some (n, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:app_response_failure = Obj.magic v in
  v

let rec decode_app_response d = 
  let rec loop () = 
    let ret:app_response = match Pbrt.Decoder.key d with
      | None -> failwith "None of the known key is found"
      | Some (1, _) -> (Pbrt.Decoder.empty_nested d ; Success)
      | Some (2, _) -> Failure (decode_app_response_failure (Pbrt.Decoder.nested d))
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

let rec encode_server_udp_configuration (v:server_udp_configuration) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.raft_id encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.inet4_address encoder;
  Pbrt.Encoder.key (3, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.raft_port encoder;
  Pbrt.Encoder.key (4, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.client_port encoder;
  ()

let rec encode_configuration (v:configuration) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (Raft_pb.encode_configuration v.raft_configuration) encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_server_udp_configuration x) encoder;
  ) v.servers_udp_configuration;
  Pbrt.Encoder.key (3, Pbrt.Bits64) encoder; 
  Pbrt.Encoder.float_as_bits64 v.compaction_period encoder;
  Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.log_record_directory encoder;
  Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.compaction_directory encoder;
  ()

let rec encode_log_entry (v:log_entry) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.request_id encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.bytes v.data encoder;
  ()

let rec encode_client_request (v:client_request) encoder = 
  match v with
  | Add_log x -> (
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_log_entry x) encoder;
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
  | Add_log_replication_failure -> (
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  )
  | Add_log_not_a_leader x -> (
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_client_response_add_log_not_aleader x) encoder;
  )

let rec encode_app_request (v:app_request) encoder = 
  match v with
  | Validate_log x -> (
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_log_entry x) encoder;
  )
  | Commit_log x -> (
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_log_entry x) encoder;
  )

let rec encode_app_response_failure (v:app_response_failure) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.error_message encoder;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.error_code encoder;
  ()

let rec encode_app_response (v:app_response) encoder = 
  match v with
  | Success -> (
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  )
  | Failure x -> (
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_app_response_failure x) encoder;
  )

let rec pp_server_udp_configuration fmt (v:server_udp_configuration) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "raft_id" Pbrt.Pp.pp_int fmt v.raft_id;
    Pbrt.Pp.pp_record_field "inet4_address" Pbrt.Pp.pp_string fmt v.inet4_address;
    Pbrt.Pp.pp_record_field "raft_port" Pbrt.Pp.pp_int fmt v.raft_port;
    Pbrt.Pp.pp_record_field "client_port" Pbrt.Pp.pp_int fmt v.client_port;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_configuration fmt (v:configuration) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "raft_configuration" Raft_pb.pp_configuration fmt v.raft_configuration;
    Pbrt.Pp.pp_record_field "servers_udp_configuration" (Pbrt.Pp.pp_list pp_server_udp_configuration) fmt v.servers_udp_configuration;
    Pbrt.Pp.pp_record_field "compaction_period" Pbrt.Pp.pp_float fmt v.compaction_period;
    Pbrt.Pp.pp_record_field "log_record_directory" Pbrt.Pp.pp_string fmt v.log_record_directory;
    Pbrt.Pp.pp_record_field "compaction_directory" Pbrt.Pp.pp_string fmt v.compaction_directory;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_log_entry fmt (v:log_entry) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "request_id" Pbrt.Pp.pp_string fmt v.request_id;
    Pbrt.Pp.pp_record_field "data" Pbrt.Pp.pp_bytes fmt v.data;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_client_request fmt (v:client_request) =
  match v with
  | Add_log x -> Format.fprintf fmt "@[Add_log(%a)@]" pp_log_entry x

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
  | Add_log_replication_failure  -> Format.fprintf fmt "Add_log_replication_failure"
  | Add_log_not_a_leader x -> Format.fprintf fmt "@[Add_log_not_a_leader(%a)@]" pp_client_response_add_log_not_aleader x

let rec pp_app_request fmt (v:app_request) =
  match v with
  | Validate_log x -> Format.fprintf fmt "@[Validate_log(%a)@]" pp_log_entry x
  | Commit_log x -> Format.fprintf fmt "@[Commit_log(%a)@]" pp_log_entry x

let rec pp_app_response_failure fmt (v:app_response_failure) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "error_message" Pbrt.Pp.pp_string fmt v.error_message;
    Pbrt.Pp.pp_record_field "error_code" Pbrt.Pp.pp_int fmt v.error_code;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_app_response fmt (v:app_response) =
  match v with
  | Success  -> Format.fprintf fmt "Success"
  | Failure x -> Format.fprintf fmt "@[Failure(%a)@]" pp_app_response_failure x
