(** IPC for the Client <-> RAFT Server Protocol *)
 
(** The IPC is based on TCP and using Raft_com_pb protobuf types *)

(** {2 Types} *)

type handle 
(** A handle to the client connection so that response can be dispatched
    to the right client. *)

type client_request = Raft_com_pb.client_request * handle
(** A client request is made of both of the message received and its 
    associated handle which can then be used to send the corresponding 
    response *)

type client_response = Raft_com_pb.client_response * handle  
(** Client response *)

type t 
(** Client IPC type *)

(** {2 Creators} *)

val make : 
  Raft_com_conf.t ->
  int -> 
  t
(** [make configuration stats server_id] initializes the 
    TCP based server IPC to process incoming client request to the RAFT
    server. *)

(** {2 Communication API} *)

val get_next : t -> client_request list Lwt.t 
(** [get_next client_ipc] returns a promise to the next set of client 
    requests *)

val send : t -> client_response list -> unit 
(** [send client_ipc client_responses] enqueues the [client_responses] for them
    to be sent asynchronously *) 
