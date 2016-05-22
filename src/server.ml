module Raft  = Raft_pb
module Udp   = Raft_udp_pb
module Conf  = Raft_udp_conf
module Perf  = Raft_udp_counter.Perf
module Stats = Raft_udp_serverstats

open Lwt.Infix

open Lwt_log_core

(* -- Server -- *)

let run_server configuration id logger print_header =

  let get_now =
    (* 
     * For easier logging the time is initialized at the 
     * beginning of the program.
     *)
    let t0 = Unix.gettimeofday () in
    (fun () ->
      Unix.gettimeofday () -. t0
    )
  in
  
  let initial_raft_state = Raft_helper.Follower.create
    ~configuration:configuration.Udp.raft_configuration ~now:(get_now ()) ~id () in

  let stats = 
    let print_header = if print_header then Some () else None in
    Stats.make 
      ?print_header 
      ~initial_log_size:initial_raft_state.Raft.log_size 
      ~id ()
  in

  let get_next_raft_message =
    Raft_udp_ipc.get_next_raft_message_f_for_server configuration id
  in

  let send_raft_message =
    let ipc_handle, f = Raft_udp_ipc.get_send_raft_message_f configuration in 
    fun ((msg, server_id) as msg_to_send) -> 
      Stats.tick_raft_msg_send stats; 
      Raft_udp_log.print_msg_to_send logger id msg server_id 
      >|= (fun () -> f ipc_handle msg_to_send)
  in

  let send_raft_messages requests =
    Lwt_list.map_p (fun msg_to_send ->
      send_raft_message msg_to_send
    ) requests
    >|= ignore
  in

  let get_next_client_request, send_client_response = 
    let req_stream, send_response_f = 
      Raft_udp_clientipc.client_request_stream logger configuration stats id 
    in 
    (
      (fun () -> Lwt_stream.get req_stream >|= (fun x -> `Client_request x)), 
      send_response_f
    )
  in 

  let next_raft_message_or_timeout timeout timeout_type = 
    Lwt.catch (fun () ->
      Lwt.pick [get_next_raft_message (); Lwt.no_cancel @@ Lwt_unix.timeout timeout]
    ) (* with *) (function
      | Lwt_unix.Timeout -> Lwt.return (`Timeout timeout_type) 
      | _                -> Lwt.return `Failure
    )
  in

  let rec server_loop threads raft_state =
    (*
     * [now'] is the time associated with [timeout] meanding that the
     * time deadline at which the [Timeout] exception will be raised should be
     * [now' + timeout].
     *
     *)

    Lwt.nchoose_split threads 

    >>=(fun (events, non_terminated_threads) ->

      let handle_follow_up_action raft_state =   
        let now = get_now () in
        let {Raft.timeout; timeout_type } = Raft_helper.Timeout_event.next raft_state now in
        let threads =
          if List.exists (function | `Client_request _ -> true  | _ -> false) events
          then [
            get_next_client_request (); 
            next_raft_message_or_timeout timeout timeout_type
          ]
          else (next_raft_message_or_timeout timeout timeout_type)::non_terminated_threads
        in  
        server_loop threads raft_state 
      in
      
      let now = get_now () in
      
      Stats.set_log_count stats raft_state.Raft.log_size;

      let raft_state = Lwt_list.fold_left_s (fun raft_state event ->
        match event with
        | `Raft_message msg -> (
          Raft_udp_serveripc.handle_raft_message ~logger ~stats ~now raft_state msg 
          >>=(fun (raft_state, msg_to_send, client_responses) ->
            send_raft_messages msg_to_send
            >|=(fun () -> raft_state)
          )
        )

        | `Timeout timeout_type -> (
          Raft_udp_serveripc.handle_timeout ~logger ~stats ~now raft_state timeout_type
          >>=(fun (raft_state, msg_to_send, client_responses) ->
           send_raft_messages msg_to_send 
           >|=(fun () -> raft_state) 
          )
        )

        | `Failure -> (
          exit 1
        )

        | `Client_request None -> (
          log ~logger ~level:Notice "None receive from stream"
          >|=(fun () -> raft_state)
        )

        | `Client_request (Some client_request) -> (
          Raft_udp_serveripc.handle_client_request ~logger ~stats ~now raft_state client_request
          >>=(fun (raft_state, msg_to_send, client_responses) ->

            List.iter (fun client_response -> 
              send_client_response (Some client_response)
            ) client_responses;

            send_raft_messages msg_to_send 
            >|=(fun () -> raft_state) 
          )
        )
        ) raft_state events in 
        raft_state >>= handle_follow_up_action 
    )
  in

  let {
    Raft.timeout; 
    timeout_type
  } = Raft_helper.Timeout_event.next initial_raft_state (get_now ()) in 

  let initial_threads = [
    get_next_client_request (); 
    next_raft_message_or_timeout timeout timeout_type;
  ] in 

  server_loop initial_threads initial_raft_state 


let run configuration id print_header = 
  let t = 
    let file_name = Printf.sprintf "raft_upd_%i.log" id in 
    Lwt_log.file ~mode:`Truncate ~file_name () 
    >>=(fun logger -> 
      run_server configuration id logger print_header 
    ) 
  in
  Lwt_main.run t 

let () =
  Printf.printf ">>PID: %i\n%!" (Unix.getpid ());
  Random.self_init ();
  let configuration = Conf.default_configuration () in

  let id   = ref (-1) in
  let id_spec = Arg.Symbol (["0";"1";"2"], fun s ->
    id := int_of_string s
  ) in

  let print_header = ref false in 
  let print_header_spec = Arg.Set print_header in

  Arg.parse [
    ("--id", id_spec , " : server raft id");
    ("--print-header", print_header_spec, " : enable header printing");
  ] (fun _ -> ()) "test.ml";

  run configuration !id !print_header
