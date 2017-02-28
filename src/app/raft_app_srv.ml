open Lwt.Infix 
open !Lwt_log_core

module APb = Raft_com_pb
module Pb_util = Raft_udp_pbutil
module Conf = Raft_com_conf

module U  = Lwt_unix 

type state = int 
(* last log index processed *)

let initial_state = 0 
(* we currently assume that the last index processed when the server start 
 * is 0. Since right now we don't support snapshotting the state of the 
 * application, everytime the app restart it starts from scratch. However
 * in the future we would like to save the state and its associated 
 * log index *)

type connection = (Lwt_io.input_channel * Lwt_unix.file_descr * bytes)  

module  Event = struct 

  type e = 
    | Failure        of string 
      (** Fatal failure of the IPC *)

    | New_connection of connection
      (** TCP IPC accepted a new connection *)

    | New_request    of APb.app_request * connection
      (** New request successfully received and decoded *)

    | App_response of APb.app_response * connection * state  

    | Connection_close 
      (** A connection was closed *)
    
    | Response_sent of connection 

  let close_connection (ic, _, _) () = 
    Lwt_io.close ic 
    >|= (fun () -> Connection_close) 

  let response_sent connection () = 
    Response_sent connection

  let new_connection connection () = 
    New_connection connection

  let new_request app_request connection () = 
    New_request (app_request, connection) 
end 

let get_next_connection_f {Conf.app_server_port; _} server_id =

  let port = 
    match List.nth app_server_port server_id with
    | p -> p 
    | exception _ -> assert(false)
  in 

  (* Initial, done once, connection setup
   *) 
  let ad = U.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", port) in 
  let fd = U.socket U.PF_INET U.SOCK_STREAM 0 in 
  U.setsockopt fd U.SO_REUSEADDR true;
  U.bind fd ad; 
  U.listen fd 1; 

  (* Function to keep accepting new connections
   *)
  fun () -> 
    Lwt.catch (fun () ->
      U.accept fd
      >>=(fun (fd2, ad) ->
        let ic = Lwt_io.of_fd ~mode:Lwt_io.input fd2 in 
        log_f ~level:Notice 
              "New connection accepted, details: %s" 
              (Raft_utl_unix.string_of_sockaddr ad)
        >|= Event.new_connection (ic, fd2, Bytes.create 1024)
      )
    ) (* with *) (fun exn ->
      let error = 
        Printf.sprintf "Accept failed: %s\n" (Printexc.to_string exn) 
      in 
      Lwt.return (Event.Failure error)  
    )

let decode_request bytes = 
  let decoder = Pbrt.Decoder.of_bytes bytes in 
  APb.decode_app_request decoder 

let next_request ((ic, fd, buffer) as connection) =
  Lwt.catch (fun () ->
    Raft_utl_connection.read_msg_with_header ic buffer
    >>= (fun (buffer', len) -> 
      log_f ~level:Notice "Request received, size: %i" len
      >>=(fun () -> 
        match decode_request (Bytes.sub buffer' 0 len) with
        | app_request ->
          let connection = 
            if buffer == buffer' 
            then connection
            else (ic, fd, buffer')
          in 
          log_f ~level:Notice 
                "Request decoded: %s" 
                (Pb_util.string_of_app_request app_request)
          >|= Event.new_request app_request connection

        | exception exn -> 
          log_f ~level:Error 
                "Failed to decode request, details: %s" 
                (Printexc.to_string exn)
          >>= Event.close_connection connection
      )
    ) 
  ) (* with *) (fun exn -> 
    log_f ~level:Error 
          "Failed to read new request, details: %s" 
          (Printexc.to_string exn)
    >>= Event.close_connection connection
  ) 

let send_app_response ((_, fd, _) as connection) app_response = 
  (* Encode to bytes *)
  let bytes = 
    let encoder = Pbrt.Encoder.create () in 
    APb.encode_app_response  app_response encoder; 
    Pbrt.Encoder.to_bytes encoder
  in
  
  Lwt.catch (fun () -> 
    Raft_utl_connection.write_msg_with_header fd bytes 
    >>=(fun () -> 
      log_f ~level:Notice 
            "Response sent %s"
            (Pb_util.string_of_app_response app_response)
      >|= Event.response_sent connection
    )
  ) (* catch *) (fun exn -> 
    log_f ~level:Error 
      "Failed to send response, details: %s, response: %s" 
      (Printexc.to_string exn) 
      (Pb_util.string_of_app_response app_response) 
    >>= Event.close_connection connection
  ) 

let server_loop configuration server_id handle_app_request () =

  let next_connection = 
    get_next_connection_f configuration server_id 
  in 

  let rec aux (threads, state) = 
    assert([] <> threads); 
      (* There should always be 1 thread to listen to new connection *)

    Lwt.nchoose_split threads 
    >>=(fun (events, non_terminated_threads)  -> 

      List.fold_left (fun (non_terminated_threads, state) -> function 
        | Event.Failure error -> (
          Printf.eprintf "App.native: Error, details: %s\n%!" error; 
          exit 1
        ) 

        | Event.New_connection connection -> 
          let non_terminated_threads = 
            (next_connection ())::
            (next_request connection)::
            non_terminated_threads 
          in 
          (non_terminated_threads, state)

        | Event.New_request (request, connection) -> 
          let t = 
            handle_app_request state request 
            >|=(fun (response, state) -> 
              Event.App_response (response, connection, state)
              (* TODO: just call send_app_response and remove 
               * Event.App_response *)
            )
          in 
          (t::non_terminated_threads, state) 

        | Event.App_response (response, connection, state) -> 
          let  non_terminated_threads = 
            (send_app_response connection response)::non_terminated_threads
          in 
          (non_terminated_threads, state)

        | Event.Connection_close -> 
          (non_terminated_threads, state)
        
        | Event.Response_sent connection -> 
          let non_terminated_threads = 
            (next_request connection)::non_terminated_threads
          in
          (non_terminated_threads, state)

      ) (non_terminated_threads, state) events
      |> aux 
    )
  in
  aux ([next_connection ()], initial_state) 

module type App_sig  = sig 

  type data  
  val decode : bytes -> data

  type result 
  val encode : result -> bytes 
end

module Make(App:App_sig) = struct 
  
  type log = {
    id : string; 
    index : int; 
    app_data : App.data;
  } 
  
  type log_result = {
    id : string; 
    index : int; 
    app_result : App.result option; 
  } 
  
  type add_log_entries = log list * (log_result list -> unit) 

  let decode_log {Raft_pb.id; index; data; _} = 
    {id; index; app_data = App.decode data}

  let handle_app_request request_push = fun state request ->
    match request with
    | APb.Add_log_entries {APb.log_entries} ->
      let log_entries  = List.map decode_log log_entries in 
      let results_t, results_u = Lwt.wait () in  
      request_push (Some (log_entries,  (fun r -> Lwt.wakeup results_u r)));
      results_t
      >|=(fun results -> 
        let state = 
          let rec aux = function
            | [] -> assert(false) 
            | {index; _} :: [] -> index 
            | _::tl -> aux tl 
          in 
          aux results
        in 
        let results = List.map (fun {id; index; app_result} ->
          let result_data = 
            match app_result with
            | None -> None
            | Some result -> Some (App.encode result) 
          in
          {APb.index; id; result_data;}
        ) results in
        (APb.(Add_log_results {results; last_log_index = state}), state) 
      )
    | APb.Init -> 
      Lwt.return (
        APb.(Add_log_results { results = []; last_log_index = state}), 
        state
      )

  let start configuration server_id =
     let (
       requests_stream, 
       requests_push, 
       requests_set_ref
     ) = Lwt_stream.create_with_reference () in 
     let server_t = 
        let handle_app_request = handle_app_request requests_push in 
        server_loop configuration server_id handle_app_request (); 
     in 
     requests_set_ref server_t;
     requests_stream

end 
