module Test = Raft_clt 
(*

let unique_request_id = ref 0 

let test_add_log_request () = 
  incr unique_request_id; 
  APb.(Add_tx {
    tx_id = Printf.sprintf "%06i|%09i" (Unix.getpid()) !unique_request_id;
    tx_data = Bytes.of_string (String.make (Random.int 10) 'a');
  })

let rec server_loop logger state count e =

  match e with 
  | Event.Failure context -> (
    Printf.eprintf "Exiting: %s\n%!" context; 
    (exit 1 : unit); 
    Lwt.return_unit;
  )

  | Event.Connection_established fd ->
    let state = State.establish state fd in 
    log_f ~logger ~level:Notice "Connection established, %s" (State.string_of_state state) 
    >>=(fun () ->
      send_request logger state @@ test_add_log_request () 
      >>= server_loop logger state count
    ) 

  | Event.Response APb.Add_log_success -> 
    begin if (count mod 1000) = 0
    then 
      Lwt_io.printlf "Success ... [%10i] [%s]" count (State.string_of_state state) 
    else 
      Lwt.return_unit
    end
    >>=(fun () -> U.sleep 0.0)
    >>=(fun () ->
      send_request logger state @@ test_add_log_request ()
      >>= server_loop logger state (count + 1)
    ) 
  
  | Event.Response APb.Add_log_validation_failure -> 
    server_loop logger state count @@ Event.failure "Validation failure" ()

  | Event.Response APb.Add_log_not_a_leader {APb.leader_id;} -> 
    Lwt_io.printlf "Not a leader received, leader hint : %s"
      @@ (Ext.string_of_option string_of_int leader_id ) 
    >>=(fun () -> 
      begin match State.fd state with
      | None -> server_loop logger state count @@ Event.failure "No connection (invariant violation)"  ()
      | Some fd -> 
        U.close fd 
        >>=(fun () ->

          let state, leader_id = match leader_id with
            | None -> State.next state
            | Some leader_id -> 
              State.potential state leader_id, leader_id 
          in

          new_connection ~to_:leader_id state 
          >>= server_loop logger state count 
        ) 
      end
    )
  
  | Event.Connection_closed ->
    let state, leader_id = State.next state in  
    U.sleep 0.25
    >>=(fun () -> new_connection ~to_:leader_id state )
    >>= server_loop logger state count 

  | Event.Init ->
    let state, leader_id = State.next state in 
    new_connection ~to_:leader_id state 
    >>= server_loop logger state count 

let main log () = 
  begin 
    if log 
    then 
      let file_name = Printf.sprintf "client%i.log" (Unix.getpid ()) in 
      let template  = "$(date).$(milliseconds) [$(level)] [$(section)] : $(message)" in
      Lwt_log.file ~mode:`Truncate ~template ~file_name ()
    else 
      Lwt.return Lwt_log_core.null 
  end 
  >>=(fun logger -> server_loop logger (State.make ()) 0 Event.Init)

let () =
  let log = ref false in 
  let log_spec = Arg.Set log  in
  
  Arg.parse [
    ("--log", log_spec, " : enable logging");
  ] (fun _ -> ()) "client.ml";

  Random.self_init ();
  Lwt_main.run (main !log ())
*)
