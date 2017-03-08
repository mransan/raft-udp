(** Configuration utilities. *)

(** {2 Types} *)

type server_ipc_configuration = {
  raft_id : int;
  inet4_address : string;
  raft_port : int;
  client_port : int;
}

type t = {
  raft_configuration : Raft_types.configuration;
  servers_ipc_configuration : server_ipc_configuration list;
  storage_directory : string;
  app_server_port : int list;
    (* TODO move this to the server_ipc_configuration *)
  client_rate_limit : int; 
}

(** {2 Utilities} *)

val default_configuration : 
  [`Gcp | `Mac] -> 
  t

val env_arg : ([`Gcp | `Mac] ref * Arg.spec)  

val string_of_env : [`Gcp |`Mac] -> string 

val sockaddr_of_server_id : 
  [< `Client | `Raft ] ->
  t ->
  int -> 
  Unix.sockaddr option

val sockaddr_of_server_config: 
  [< `Client | `Raft ] ->
  server_ipc_configuration->
  Unix.sockaddr
