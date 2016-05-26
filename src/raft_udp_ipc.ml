module Conf = Raft_udp_conf
module RPb  = Raft_pb
module Pb   = Raft_udp_pb

open Lwt.Infix 

type event = 
  | Raft_message of Raft_pb.message
  | Failure 

type next_raft_message_f = unit -> event Lwt.t  

let get_next_raft_message_f_for_server configuration server_id =
  
  match Conf.sockaddr_of_server_id `Raft configuration server_id with
  | None ->
    (fun () -> Lwt.return Failure)

  | Some ad ->

    let module U = Lwt_unix in
    let fd = U.socket U.PF_INET U.SOCK_DGRAM 0 in
    U.bind fd ad;

    let buffer_size = 1024 * 1024 in
    let buffer = Bytes.create buffer_size in

    let rec receive_loop () =
      U.recvfrom fd buffer 0 buffer_size []
      >|= (fun (nb_of_bytes_received, _) ->
        let decoder = Pbrt.Decoder.of_bytes buffer in
        Raft_message (RPb.decode_message decoder)
      )
    in
    receive_loop

type ipc_handle = (RPb.message *  int) Lwt_stream.t  

type send_raft_message_f = 
  ipc_handle ->
  RPb.message * int ->
  unit 

let get_send_raft_message_f configuration =

  (*
   * All the outgoing RAFT messages are sent in a dedicated
   * concurrent threads. The RAFT protocol nevers requires
   * the sender of a RAFT message to block and wait for the response. (In fact
   * that response might never come).
   *
   * In order to send those message concurrently, an [Lwt_stream] is used. 
   * 
   * For the caller of this API sending a response is simply pushing the 
   * response to the stream (immediate). The Lwt scheduler will then 
   * pick it up asynchronously.
   *
   *)

  let module U = Lwt_unix in

  let server_addresses = List.map (fun ({Pb.raft_id} as server_config) ->
    let fd = U.socket U.PF_INET U.SOCK_DGRAM 0 in
    (raft_id, (Conf.sockaddr_of_server_config `Raft server_config, fd))
  ) configuration.Pb.servers_udp_configuration in

  let res_stream, res_push, res_set_ref = Lwt_stream.create_with_reference () in 

  let res_stream' : unit Lwt.t = Lwt_stream.iter_p (fun (msg, server_id) ->
    match List.assq server_id server_addresses with
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
          | nb_bytes when nb_bytes = remaining ->
            Lwt.return_unit
            (* All good message is delivered
             *)
          | nb_bytes -> 
            sendto (from + nb_bytes) (remaining - nb_bytes)
        )
      in
      sendto 0 buffer_size
    )
    | exception Not_found -> 
      Lwt.fail_with @@ Printf.sprintf "Address not found for server %i" server_id
  ) res_stream
  in  

  res_set_ref res_stream'; 
  
  (* In order to keep alive the [res_stream] (ie not being garbage collected), we 
   * return the stream as an abstract type to the caller and impose the requirement 
   * to give it as an argument to the [send] function. 
   *
   * While this argument is ignore the fact that it is mandatory forces the caller to 
   * keep it in its application.
   *)
  
  (res_stream, (fun _ msg_to_send ->
    res_push (Some msg_to_send)
  ))

