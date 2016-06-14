[@@@ocaml.warning "-30"]

type server_ipc_configuration = {
  raft_id : int;
  inet4_address : string;
  raft_port : int;
  client_port : int;
}

and server_ipc_configuration_mutable = {
  mutable raft_id : int;
  mutable inet4_address : string;
  mutable raft_port : int;
  mutable client_port : int;
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
  app_server_port : int;
}

and configuration_mutable = {
  mutable raft_configuration : Raft_pb.configuration;
  mutable servers_ipc_configuration : server_ipc_configuration list;
  mutable disk_backup : disk_backup_configuration;
  mutable app_server_port : int;
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
  | Add_log_validation_failure
  | Add_log_not_a_leader of client_response_add_log_not_aleader

type app_ipc_debug = {
  raft_server_id : int;
  debug_id : int;
}

and app_ipc_debug_mutable = {
  mutable raft_server_id : int;
  mutable debug_id : int;
}

type app_request_validate_logs = {
  log_entries : log_entry list;
}

and app_request_validate_logs_mutable = {
  mutable log_entries : log_entry list;
}

type app_request_app_request_payload =
  | Validate_logs of app_request_validate_logs
  | Commit_log of log_entry

and app_request = {
  app_request_debug_info : app_ipc_debug;
  app_request_payload : app_request_app_request_payload;
}

and app_request_mutable = {
  mutable app_request_debug_info : app_ipc_debug;
  mutable app_request_payload : app_request_app_request_payload;
}

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
  request_id : string;
  result : app_response_validation_result;
}

and app_response_validation_mutable = {
  mutable request_id : string;
  mutable result : app_response_validation_result;
}

type app_response_validations = {
  validations : app_response_validation list;
}

and app_response_validations_mutable = {
  mutable validations : app_response_validation list;
}

type app_response_commit_log_ack = {
  request_id : string;
}

and app_response_commit_log_ack_mutable = {
  mutable request_id : string;
}

type app_response_app_response_payload =
  | Validations of app_response_validations
  | Commit_log_ack of app_response_commit_log_ack

and app_response = {
  app_response_debug_info : app_ipc_debug;
  app_response_payload : app_response_app_response_payload;
}

and app_response_mutable = {
  mutable app_response_debug_info : app_ipc_debug;
  mutable app_response_payload : app_response_app_response_payload;
}

let rec default_server_ipc_configuration 
  ?raft_id:((raft_id:int) = 0)
  ?inet4_address:((inet4_address:string) = "")
  ?raft_port:((raft_port:int) = 0)
  ?client_port:((client_port:int) = 0)
  () : server_ipc_configuration  = {
  raft_id;
  inet4_address;
  raft_port;
  client_port;
}

and default_server_ipc_configuration_mutable () : server_ipc_configuration_mutable = {
  raft_id = 0;
  inet4_address = "";
  raft_port = 0;
  client_port = 0;
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
  ?app_server_port:((app_server_port:int) = 0)
  () : configuration  = {
  raft_configuration;
  servers_ipc_configuration;
  disk_backup;
  app_server_port;
}

and default_configuration_mutable () : configuration_mutable = {
  raft_configuration = Raft_pb.default_configuration ();
  servers_ipc_configuration = [];
  disk_backup = default_disk_backup_configuration ();
  app_server_port = 0;
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

let rec default_app_ipc_debug 
  ?raft_server_id:((raft_server_id:int) = 0)
  ?debug_id:((debug_id:int) = 0)
  () : app_ipc_debug  = {
  raft_server_id;
  debug_id;
}

and default_app_ipc_debug_mutable () : app_ipc_debug_mutable = {
  raft_server_id = 0;
  debug_id = 0;
}

let rec default_app_request_validate_logs 
  ?log_entries:((log_entries:log_entry list) = [])
  () : app_request_validate_logs  = {
  log_entries;
}

and default_app_request_validate_logs_mutable () : app_request_validate_logs_mutable = {
  log_entries = [];
}

let rec default_app_request_app_request_payload () : app_request_app_request_payload = Validate_logs (default_app_request_validate_logs ())

and default_app_request 
  ?app_request_debug_info:((app_request_debug_info:app_ipc_debug) = default_app_ipc_debug ())
  ?app_request_payload:((app_request_payload:app_request_app_request_payload) = Validate_logs (default_app_request_validate_logs ()))
  () : app_request  = {
  app_request_debug_info;
  app_request_payload;
}

and default_app_request_mutable () : app_request_mutable = {
  app_request_debug_info = default_app_ipc_debug ();
  app_request_payload = Validate_logs (default_app_request_validate_logs ());
}

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
  ?request_id:((request_id:string) = "")
  ?result:((result:app_response_validation_result) = Success)
  () : app_response_validation  = {
  request_id;
  result;
}

and default_app_response_validation_mutable () : app_response_validation_mutable = {
  request_id = "";
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

let rec default_app_response_commit_log_ack 
  ?request_id:((request_id:string) = "")
  () : app_response_commit_log_ack  = {
  request_id;
}

and default_app_response_commit_log_ack_mutable () : app_response_commit_log_ack_mutable = {
  request_id = "";
}

let rec default_app_response_app_response_payload () : app_response_app_response_payload = Validations (default_app_response_validations ())

and default_app_response 
  ?app_response_debug_info:((app_response_debug_info:app_ipc_debug) = default_app_ipc_debug ())
  ?app_response_payload:((app_response_payload:app_response_app_response_payload) = Validations (default_app_response_validations ()))
  () : app_response  = {
  app_response_debug_info;
  app_response_payload;
}

and default_app_response_mutable () : app_response_mutable = {
  app_response_debug_info = default_app_ipc_debug ();
  app_response_payload = Validations (default_app_response_validations ());
}

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
    | Some (n, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
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
    | Some (n, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
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
    | Some (6, Pbrt.Varint) -> (
      v.app_server_port <- Pbrt.Decoder.int_as_varint d;
      loop ()
    )
    | Some (6, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(configuration), field(6)", pk))
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

let rec decode_app_ipc_debug d =
  let v = default_app_ipc_debug_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Varint) -> (
      v.raft_server_id <- Pbrt.Decoder.int_as_varint d;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(app_ipc_debug), field(1)", pk))
    )
    | Some (2, Pbrt.Varint) -> (
      v.debug_id <- Pbrt.Decoder.int_as_varint d;
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(app_ipc_debug), field(2)", pk))
    )
    | Some (n, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:app_ipc_debug = Obj.magic v in
  v

let rec decode_app_request_validate_logs d =
  let v = default_app_request_validate_logs_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
      v.log_entries <- List.rev v.log_entries;
    )
    | Some (1, Pbrt.Bytes) -> (
      v.log_entries <- (decode_log_entry (Pbrt.Decoder.nested d)) :: v.log_entries;
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(app_request_validate_logs), field(1)", pk))
    )
    | Some (n, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:app_request_validate_logs = Obj.magic v in
  v

let rec decode_app_request_app_request_payload d = 
  let rec loop () = 
    let ret:app_request_app_request_payload = match Pbrt.Decoder.key d with
      | None -> failwith "None of the known key is found"
      | Some (3, _) -> Validate_logs (decode_app_request_validate_logs (Pbrt.Decoder.nested d))
      | Some (4, _) -> Commit_log (decode_log_entry (Pbrt.Decoder.nested d))
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

and decode_app_request d =
  let v = default_app_request_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Bytes) -> (
      v.app_request_debug_info <- decode_app_ipc_debug (Pbrt.Decoder.nested d);
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(app_request), field(1)", pk))
    )
    | Some (3, Pbrt.Bytes) -> (
      v.app_request_payload <- Validate_logs (decode_app_request_validate_logs (Pbrt.Decoder.nested d));
      loop ()
    )
    | Some (3, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(app_request), field(3)", pk))
    )
    | Some (4, Pbrt.Bytes) -> (
      v.app_request_payload <- Commit_log (decode_log_entry (Pbrt.Decoder.nested d));
      loop ()
    )
    | Some (4, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(app_request), field(4)", pk))
    )
    | Some (n, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:app_request = Obj.magic v in
  v

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
      v.request_id <- Pbrt.Decoder.string d;
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

let rec decode_app_response_commit_log_ack d =
  let v = default_app_response_commit_log_ack_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (2, Pbrt.Bytes) -> (
      v.request_id <- Pbrt.Decoder.string d;
      loop ()
    )
    | Some (2, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(app_response_commit_log_ack), field(2)", pk))
    )
    | Some (n, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:app_response_commit_log_ack = Obj.magic v in
  v

let rec decode_app_response_app_response_payload d = 
  let rec loop () = 
    let ret:app_response_app_response_payload = match Pbrt.Decoder.key d with
      | None -> failwith "None of the known key is found"
      | Some (4, _) -> Validations (decode_app_response_validations (Pbrt.Decoder.nested d))
      | Some (5, _) -> Commit_log_ack (decode_app_response_commit_log_ack (Pbrt.Decoder.nested d))
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

and decode_app_response d =
  let v = default_app_response_mutable () in
  let rec loop () = 
    match Pbrt.Decoder.key d with
    | None -> (
    )
    | Some (1, Pbrt.Bytes) -> (
      v.app_response_debug_info <- decode_app_ipc_debug (Pbrt.Decoder.nested d);
      loop ()
    )
    | Some (1, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(app_response), field(1)", pk))
    )
    | Some (4, Pbrt.Bytes) -> (
      v.app_response_payload <- Validations (decode_app_response_validations (Pbrt.Decoder.nested d));
      loop ()
    )
    | Some (4, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(app_response), field(4)", pk))
    )
    | Some (5, Pbrt.Bytes) -> (
      v.app_response_payload <- Commit_log_ack (decode_app_response_commit_log_ack (Pbrt.Decoder.nested d));
      loop ()
    )
    | Some (5, pk) -> raise (
      Protobuf.Decoder.Failure (Protobuf.Decoder.Unexpected_payload ("Message(app_response), field(5)", pk))
    )
    | Some (n, payload_kind) -> Pbrt.Decoder.skip d payload_kind; loop ()
  in
  loop ();
  let v:app_response = Obj.magic v in
  v

let rec encode_server_ipc_configuration (v:server_ipc_configuration) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.raft_id encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.inet4_address encoder;
  Pbrt.Encoder.key (3, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.raft_port encoder;
  Pbrt.Encoder.key (4, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.client_port encoder;
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
  Pbrt.Encoder.key (6, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.app_server_port encoder;
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
  | Add_log_validation_failure -> (
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.empty_nested encoder
  )
  | Add_log_not_a_leader x -> (
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_client_response_add_log_not_aleader x) encoder;
  )

let rec encode_app_ipc_debug (v:app_ipc_debug) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.raft_server_id encoder;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.debug_id encoder;
  ()

let rec encode_app_request_validate_logs (v:app_request_validate_logs) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_log_entry x) encoder;
  ) v.log_entries;
  ()

let rec encode_app_request_app_request_payload (v:app_request_app_request_payload) encoder = 
  match v with
  | Validate_logs x -> (
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_app_request_validate_logs x) encoder;
  )
  | Commit_log x -> (
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_log_entry x) encoder;
  )

and encode_app_request (v:app_request) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_app_ipc_debug v.app_request_debug_info) encoder;
  (
    match v.app_request_payload with
    | Validate_logs x -> (
      Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
      Pbrt.Encoder.nested (encode_app_request_validate_logs x) encoder;
    )
    | Commit_log x -> (
      Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
      Pbrt.Encoder.nested (encode_log_entry x) encoder;
    )
  );
  ()

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
  Pbrt.Encoder.string v.request_id encoder;
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

let rec encode_app_response_commit_log_ack (v:app_response_commit_log_ack) encoder = 
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.request_id encoder;
  ()

let rec encode_app_response_app_response_payload (v:app_response_app_response_payload) encoder = 
  match v with
  | Validations x -> (
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_app_response_validations x) encoder;
  )
  | Commit_log_ack x -> (
    Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_app_response_commit_log_ack x) encoder;
  )

and encode_app_response (v:app_response) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (encode_app_ipc_debug v.app_response_debug_info) encoder;
  (
    match v.app_response_payload with
    | Validations x -> (
      Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
      Pbrt.Encoder.nested (encode_app_response_validations x) encoder;
    )
    | Commit_log_ack x -> (
      Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
      Pbrt.Encoder.nested (encode_app_response_commit_log_ack x) encoder;
    )
  );
  ()

let rec pp_server_ipc_configuration fmt (v:server_ipc_configuration) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "raft_id" Pbrt.Pp.pp_int fmt v.raft_id;
    Pbrt.Pp.pp_record_field "inet4_address" Pbrt.Pp.pp_string fmt v.inet4_address;
    Pbrt.Pp.pp_record_field "raft_port" Pbrt.Pp.pp_int fmt v.raft_port;
    Pbrt.Pp.pp_record_field "client_port" Pbrt.Pp.pp_int fmt v.client_port;
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
    Pbrt.Pp.pp_record_field "app_server_port" Pbrt.Pp.pp_int fmt v.app_server_port;
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
  | Add_log_validation_failure  -> Format.fprintf fmt "Add_log_validation_failure"
  | Add_log_not_a_leader x -> Format.fprintf fmt "@[Add_log_not_a_leader(%a)@]" pp_client_response_add_log_not_aleader x

let rec pp_app_ipc_debug fmt (v:app_ipc_debug) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "raft_server_id" Pbrt.Pp.pp_int fmt v.raft_server_id;
    Pbrt.Pp.pp_record_field "debug_id" Pbrt.Pp.pp_int fmt v.debug_id;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_app_request_validate_logs fmt (v:app_request_validate_logs) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "log_entries" (Pbrt.Pp.pp_list pp_log_entry) fmt v.log_entries;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_app_request_app_request_payload fmt (v:app_request_app_request_payload) =
  match v with
  | Validate_logs x -> Format.fprintf fmt "@[Validate_logs(%a)@]" pp_app_request_validate_logs x
  | Commit_log x -> Format.fprintf fmt "@[Commit_log(%a)@]" pp_log_entry x

and pp_app_request fmt (v:app_request) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "app_request_debug_info" pp_app_ipc_debug fmt v.app_request_debug_info;
    Pbrt.Pp.pp_record_field "app_request_payload" pp_app_request_app_request_payload fmt v.app_request_payload;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

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
    Pbrt.Pp.pp_record_field "request_id" Pbrt.Pp.pp_string fmt v.request_id;
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

let rec pp_app_response_commit_log_ack fmt (v:app_response_commit_log_ack) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "request_id" Pbrt.Pp.pp_string fmt v.request_id;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_app_response_app_response_payload fmt (v:app_response_app_response_payload) =
  match v with
  | Validations x -> Format.fprintf fmt "@[Validations(%a)@]" pp_app_response_validations x
  | Commit_log_ack x -> Format.fprintf fmt "@[Commit_log_ack(%a)@]" pp_app_response_commit_log_ack x

and pp_app_response fmt (v:app_response) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "app_response_debug_info" pp_app_ipc_debug fmt v.app_response_debug_info;
    Pbrt.Pp.pp_record_field "app_response_payload" pp_app_response_app_response_payload fmt v.app_response_payload;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()
