open Lwt.Infix 
open !Lwt_log_core

module RPb = Raft_pb
module Conf = Raft_com_conf
module RConv = Raft_pb_conv 

module Server_stats = Raft_srv_stats

let section = Section.make (Printf.sprintf "%10s" "RaftIPC")

type event = 
  | Raft_message of Raft_pb.message
  | Failure 

type next_raft_message_f = unit -> event Lwt.t

type t = {
  outgoing_message_processing : unit Lwt.t; 
    (* Threads which sends the outgoing messages *)
  push_outgoing_message : (RPb.message * int) option -> unit;
    (* Function to push an outgoing message in the stream *)
  next_raft_message : next_raft_message_f;
    (* Functio to get the next RAFT message *)
  stats : Raft_srv_stats.t;
}

let get_next_raft_message_f_for_server configuration server_id =
  match Conf.sockaddr_of_server_id `Raft configuration server_id with
  | None ->
    (fun () -> Lwt.return Failure)
    (* TODO: investigate if we could avoid this construct and rather 
     * raise an exception or return Result.result value *)

  | Some ad ->
    let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_DGRAM 0 in
    Lwt_unix.bind fd ad;

    let buffer_size = 65536 in
    let buffer = Bytes.create buffer_size in
    (* TODO: we should have a header for this communication *)

    fun () ->
      Lwt_unix.recvfrom fd buffer 0 buffer_size []
      >|= (fun (nb_of_bytes_received, _) ->
        let decoder = 
          Pbrt.Decoder.of_bytes (Bytes.sub buffer 0 nb_of_bytes_received) 
        in
        match RPb.decode_message decoder with
        | message -> Raft_message message 
        | exception _ -> Failure 
      )

let make configuration stats server_id = 
  let server_addresses = List.map (fun ({Conf.raft_id; _ } as server_config) ->
    let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_DGRAM 0 in
    (raft_id, (Conf.sockaddr_of_server_config `Raft server_config, fd))
  ) configuration.Conf.servers_ipc_configuration in
  (* TODO replace this with an array for faster access *)

  let (
    outgoing_message_stream, 
    push_outgoing_message
  ) = Lwt_stream.create () in 

  let outgoing_message_processing = Lwt_stream.iter_s (fun (msg, server_id) ->
    (* IMPORTANT : DO NOT USE Lwt_stream.iter_p, this caused 
     * large increase in memory usage. It looks like the memory
     * is not garbage collected.  *)
    match List.assq server_id server_addresses with
    | (ad, fd) -> (
      let encoder = Pbrt.Encoder.create () in
      RPb.encode_message msg encoder;
      let buffer  = Pbrt.Encoder.to_bytes encoder in
      let buffer_size = Bytes.length buffer in

      let rec sendto from remaining =
        Lwt_unix.sendto fd buffer 0 buffer_size [] ad
        >>= (function 
          | 0 -> Lwt.return_unit
            (* We ignore the failure here, the RAFT protocol
             * supports message not being delivered and will
             * ensure that it will recover.  *)
          | nb_bytes when nb_bytes = remaining ->
            Raft_srv_debug.print_msg_to_send section msg server_id 
            (* All good message is delivered *)
          | nb_bytes -> 
            sendto (from + nb_bytes) (remaining - nb_bytes)
        )
      in
      sendto 0 buffer_size
    )
    | exception Not_found -> 
      Printf.sprintf "Address not found for server %i" server_id
      |> Lwt.fail_with
  ) outgoing_message_stream
  in  
  let next_raft_message = 
    get_next_raft_message_f_for_server configuration server_id
  in
  {
    outgoing_message_processing; 
    push_outgoing_message;
    next_raft_message; 
    stats;
  }

let send t messages  = 
  let {push_outgoing_message; stats; _ } = t in 
  List.iter (fun ((msg, server_id) ) ->
    let msg = RConv.message_to_pb msg in 
    let msg_to_send  = (msg, server_id) in 
    Server_stats.tick_raft_msg_send stats; 
    push_outgoing_message (Some msg_to_send);
  ) messages

let get_next {next_raft_message; _} = 
  next_raft_message () 
