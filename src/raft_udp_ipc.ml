module Conf = Raft_udp_conf
module Raft = Raft_pb

open Lwt.Infix 

type 'a next_raft_message_f = 
  unit -> 
  ([> `Raft_message of Raft_pb.message | `Failure] as 'a) Lwt.t  

let get_next_raft_message_f_for_server configuration server_id =
  
  match Conf.sockaddr_of_server_id configuration server_id with
  | None ->
    (fun () -> Lwt.return `Failure)
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
        `Raft_message (Raft.decode_message decoder)
      )
    in
    receive_loop

type send_raft_message_f = 
  Raft_pb.message ->
  int ->
  unit Lwt.t 

let get_send_raft_message_f configuration =

  let module U = Lwt_unix in

  let server_addresses = List.map (fun ({Raft_udp_pb.raft_id} as server_config) ->
    (raft_id, Conf.sockaddr_of_server_config server_config)
  ) configuration.Raft_udp_pb.servers_udp_configuration in

  let fd = U.socket U.PF_INET U.SOCK_DGRAM 0 in

  (fun msg server_id ->
    match List.assq server_id server_addresses with
    | ad -> (
      let encoder = Pbrt.Encoder.create () in
      Raft.encode_message msg encoder;
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
  )
