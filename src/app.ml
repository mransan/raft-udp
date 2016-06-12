open Lwt.Infix 
open Lwt_log_core

module Pb = Raft_udp_pb
module Pb_util = Raft_udp_pbutil
module U  = Lwt_unix 

let string_of_debug_info ({Pb.raft_server_id;debug_id} : Pb.app_ipc_debug) =  
  Printf.sprintf "{raft server id: %3i, unique id: %5i}" raft_server_id debug_id

let string_of_sockaddr = function
  | Unix.ADDR_UNIX addr -> 
    Printf.sprintf "ADDR_UNIX(%s)" addr
  | Unix.ADDR_INET (addr, port) -> 
    Printf.sprintf "ADDR_INET(address: %s, port: %i)" (Unix.string_of_inet_addr addr) port

module  Event = struct 

  type e = 
    | Failure        of string 
      (** Fatal failure of the IPC *)

    | New_connection of Lwt_unix.file_descr 
      (** TCP IPC accepted a new connection *)

    | New_request    of Pb.app_request * Lwt_unix.file_descr  
      (** New request successfully received and decoded *)

    | Connection_close 
      (** A connection was closed *)
    
    | Response_sent of Lwt_unix.file_descr
      (** A connection was closed *)

  let close_connection fd () = 
    U.close fd >|= (fun () -> Connection_close) 

  let response_sent fd () = 
    Response_sent fd

  let new_connection fd () = 
    New_connection fd
end 

let get_next_connection_f logger () =

  (* Initial, done once, connection setup
   *) 
  let ad = U.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", 40000) in 
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
        log_f ~logger ~level:Notice "New connection accepted, details: %s" (string_of_sockaddr ad)
        >|= Event.new_connection fd2 
      )
    ) (* with *) (fun exn ->
      let error = Printf.sprintf "Accept failed: %s\n" (Printexc.to_string exn) in 
      Lwt.return (Event.Failure error)  
    )

let decode_request bytes = 
  let decoder = Pbrt.Decoder.of_bytes bytes in 
  Pb.decode_app_request decoder 

let next_request  =

  let buffer = Bytes.create 1024 in
    (*  Only needs to create the buffer once
     *)

  fun logger fd -> 
    Lwt.catch (fun () ->
      U.read fd buffer 0 1024 
      >>=(function
        | 0 -> Event.close_connection fd () 
        | n -> 
          log_f ~logger ~level:Notice "Request received, size: %i" n
          >>=(fun () -> 
            let app_request = decode_request (Bytes.sub buffer 0 n) in 
            log_f ~logger ~level:Notice "Request decoded: %s" (Pb_util.string_of_app_request app_request)
            >|=(fun () -> Event.New_request (app_request, fd))
          )
      ) 
    ) (* with *) (fun exn -> 
      Event.close_connection fd () 
    ) 

let handle_validate_log {Pb.request_id; data} = 
  if Bytes.length data > 10 
  then Pb.({
    request_id; 
    result = Failure {
      error_message = "Data is too big"; 
      error_code = 1;
    }
  }) 
  else Pb.({
    request_id; 
    result = Success;
  }) 

let handle_app_request {Pb.app_request_debug_info; app_request_payload} = 
  match app_request_payload with 
  | Pb.Validate_logs {Pb.log_entries; } ->
    let validations = List.map handle_validate_log log_entries in 
    Pb.({
      app_response_debug_info = app_request_debug_info; 
      app_response_payload = Validations {validations}; 
    })

  | Pb.Commit_log {Pb.request_id; _ } -> Pb.({
    app_response_debug_info = app_request_debug_info;
    app_response_payload = Commit_log_ack {request_id;};
  })

let send_app_response logger fd app_response = 
  (* Encode to bytes *)
  let bytes = 
    let encoder = Pbrt.Encoder.create () in 
    Pb.encode_app_response  app_response encoder; 
    Pbrt.Encoder.to_bytes encoder 
  in
  let len   = Bytes.length bytes in 

  (* Send the bytes *)
  Lwt.catch (fun () -> 
    U.write fd bytes 0 len
    >>=(function
      | 0 -> Event.close_connection fd () 
      | n -> assert(len = n); 
        log_f ~logger ~level:Notice "Response sent (size: %i): %s" n 
          (Pb_util.string_of_app_response app_response)
        >|= Event.response_sent fd 
    )  
  ) (* with *) (fun exn -> 
    Event.close_connection fd () 
  ) 

let server_loop logger () =

  let next_connection = get_next_connection_f logger () in 

  let rec aux threads  = 
    assert([] <> threads); 
      (* There should always be 1 thread to listen to new connection
       *)

    Lwt.nchoose_split threads 
    >>=(fun (events, non_terminated_threads)  -> 

      List.fold_left (fun non_terminated_threads -> function 
        | Event.Failure error -> (
          Printf.eprintf "App.native: Error, details: %s\n%!" error; 
          exit 1
        ) 

        | Event.New_connection fd -> 
          (next_connection ())::(next_request logger fd)::non_terminated_threads 

        | Event.New_request (request, fd) -> 
          let response = handle_app_request request in 
          (send_app_response logger fd response)::non_terminated_threads

        | Event.Connection_close -> 
          non_terminated_threads
        
        | Event.Response_sent fd -> 
          (next_request logger fd)::non_terminated_threads

      ) non_terminated_threads events
      |> aux 
    )
  in
  aux [next_connection ()] 

let main log () = 
  begin 
    if log 
    then 
      let file_name = "app.log" in 
      let template  = "$(date).$(milliseconds) [$(level)] [$(section)] : $(message)" in
      Lwt_log.file ~mode:`Truncate ~template ~file_name ()
    else 
      Lwt.return Lwt_log_core.null
  end
  >>=(fun logger -> server_loop logger ())

let () = 
  let log = ref false in 
  let log_spec = Arg.Set log  in
  
  Arg.parse [
    ("--log", log_spec, " : enable logging");
  ] (fun _ -> ()) "test.ml";

  Sys.set_signal Sys.sigpipe Sys.Signal_ignore ; 
  Lwt_main.run (main !log ())
