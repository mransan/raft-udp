open Lwt.Infix 

module Conf = Raft_com_conf
module U = Lwt_unix 
module L = Lwt_log_core 
module RPb = Raft_pb
module UCom_pb = Raft_com_pb
module Server_stats = Raft_srv_serverstats

let section = L.Section.make (Printf.sprintf "%10s" "RaftPtc")

type t = {
  logger : Lwt_log_core.logger;
    (* Logger to be used *)
  stats : Raft_srv_serverstats.t;
    (* Stats collection *)
  outgoing_message_processing : unit Lwt.t; 
    (* Threads which sends the outgoing RAFT messages *)
  push_outgoing_message : (Raft_pb.message * int) option -> unit;
    (* Function to push an outgoing RAFT message to be sent in the stream *)
  next_message_f : unit -> (Raft_pb.message, string) result Lwt.t;
    (* Function to wait on the next RAFT message for this server *)
}

let get_next_raft_message_f_for_server configuration server_id =
  
  match Conf.sockaddr_of_server_id `Raft configuration server_id with
  | None ->
    (fun () -> Lwt.return (Error "Error creating sockaddr"))

  | Some ad ->

    let fd = U.socket U.PF_INET U.SOCK_DGRAM 0 in
    U.bind fd ad;

    let buffer_size = (1024 * 1024)  in
    let buffer = Bytes.create buffer_size in

    fun () ->
      U.recvfrom fd buffer 0 buffer_size []
      >|= (fun (nb_of_bytes_received, _) ->
        let decoder = 
          Pbrt.Decoder.of_bytes (Bytes.sub buffer 0 nb_of_bytes_received) 
        in
        Ok (RPb.decode_message decoder)
      )

let make ~logger ~stats ~configuration ~server_id () = 
  (* 
   * Create the mapping of outgoing server_id to the sockaddr
   *)
  let server_addresses = List.map (fun ({UCom_pb.raft_id; _ } as server_config) ->
    let fd = U.socket U.PF_INET U.SOCK_DGRAM 0 in
    (raft_id, (Conf.sockaddr_of_server_config `Raft server_config, fd))
  ) configuration.UCom_pb.servers_ipc_configuration in

  let (
    outgoing_message_stream, 
    push_outgoing_message
  ) = Lwt_stream.create () in 

  let outgoing_message_processing = Lwt_stream.iter_s (fun (msg, to_server_id) ->
    (* IMPORTANT : DO NOT USE Lwt_stream.iter_p, this caused 
     * large increase in memory usage. It looks like the memory
     * is not garbage collected.
     *)
    Server_stats.tick_raft_msg_send stats; 
    Raft_srv_log.print_msg_to_send logger section server_id msg to_server_id 
    >>=(fun () ->
      match List.assq to_server_id server_addresses with
      | (ad, fd) -> (
        let encoder = Pbrt.Encoder.create () in
        RPb.encode_message msg encoder;
        let buffer  = Pbrt.Encoder.to_bytes encoder in
        let buffer_size = Bytes.length buffer in

        let rec sendto from remaining =
          U.sendto fd buffer 0 buffer_size [] ad
          >>= (function 
            | 0 -> Lwt.return_unit
              (* We ignore the failure here, the RAFT protocol
               * supports message not being delivered and will
               * ensure that it will recover. 
               *)
            | nb_bytes_sent when nb_bytes_sent = remaining ->
              Lwt.return_unit
              (* All good message is delivered
               *)
            | nb_bytes_sent -> 
              sendto (from + nb_bytes_sent) (remaining - nb_bytes_sent)
          )
        in
        sendto 0 buffer_size
      )
      | exception Not_found -> 
        Lwt.fail_with @@ Printf.sprintf "Address not found for server %i" to_server_id
    )
  ) outgoing_message_stream
  in  

  {
    logger; 
    stats;
    outgoing_message_processing;
    push_outgoing_message;
    next_message_f = get_next_raft_message_f_for_server configuration server_id; 
  }

let next_message {next_message_f; _ } = 
  next_message_f () 

let send_message {push_outgoing_message; _} msg server_id = 
  push_outgoing_message (Some (msg, server_id)) 

let send_messages {push_outgoing_message; _} msgs = 
  List.iter (fun (msg, server_id) -> 
    push_outgoing_message (Some (msg, server_id))
  ) msgs 
 

