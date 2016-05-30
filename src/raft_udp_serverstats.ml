module Counter = Raft_udp_counter.Counter 
module Perf    = Raft_udp_counter.Perf

open Lwt.Infix 

type t =  {
  id : int;
  raft_msg_recv : Counter.t; 
  raft_msg_send : Counter.t; 
  log_count : Counter.t; 
  heartbeat : Counter.t; 
  append_failures: Counter.t;
  new_client_connection : Counter.t;
  client_requests : Counter.t; 
  msg_processing: Perf.t;
  hb_processing : Perf.t; 
  mutable print_stats_t : unit Lwt.t;
  print_header : bool;
}

let print_stats_t t =

  let {
    id;
    raft_msg_recv;
    raft_msg_send;
    log_count;
    heartbeat;
    new_client_connection;
    client_requests;
    msg_processing;
    append_failures;
    hb_processing; 
    print_header; 
    _
  } = t in
  
  let print_header_every = 7 in 

  let rec aux counter () =
    Lwt_unix.sleep 1.
    >>=(fun () -> 
      if counter = 0 && print_header 
      then 
        Lwt_io.printf 
          "[id] | %7s | %7s | %6s | %8s | %4s | %5s | %6s | %6s | %10s | %10s | \n" 
          "r rcv/s"
          "r snt/s" 
          "log/s"
          "log nb"
          "hb/s"
          "clt/s"
          "req/s"
          "er/s"
          "av msg(us)"
          "av hb(us)"
        >|=(fun () -> print_header_every)
      else
        Lwt.return counter 
    )
    >>=(fun counter ->
      Lwt_io.printf "[%2i] | %7.0f | %7.0f | %6.0f | %8i | %4.1f | %5.1f | %6.0f | %6.1f | %10.1f | %10.1f | \n"
        id
        (Counter.rate raft_msg_recv)
        (Counter.rate raft_msg_send)
        (Counter.rate log_count)
        (Counter.value log_count)
        (Counter.rate heartbeat)
        (Counter.rate new_client_connection)
        (Counter.rate client_requests)
        (Counter.rate append_failures)
        (Perf.avg ~reset:() ~unit_:`Us msg_processing)
        (Perf.avg ~reset:() ~unit_:`Us hb_processing)
      >>=(fun () -> aux (counter - 1) ()) 
    )
  in
  aux 0 () 

let make ?print_header ~initial_log_size ~id () = 
  let print_header = match print_header with 
    | None -> false
    | Some () -> true
  in
  let t = {
    id;
    raft_msg_send = Counter.make ();
    raft_msg_recv = Counter.make (); 
    log_count     = Counter.make ~initial_counter:initial_log_size ();
    heartbeat      = Counter.make ();
    new_client_connection = Counter.make ();
    client_requests = Counter.make ();
    msg_processing= Perf.make (); 
    hb_processing = Perf.make (); 
    append_failures = Counter.make (); 
    print_stats_t = Lwt.return_unit;
    print_header;
  } in
  t.print_stats_t <- print_stats_t t;
  t

let tick_raft_msg_send {raft_msg_send; _ } = 
  Counter.incr raft_msg_send

let tick_raft_msg_recv {raft_msg_recv; _ } = 
  Counter.incr raft_msg_recv

let set_log_count {log_count; _ } i= 
  Counter.set log_count i

let tick_heartbeat {heartbeat;_ } = 
  Counter.incr heartbeat

let tick_new_client_connection {new_client_connection;_ } = 
  Counter.incr new_client_connection

let tick_client_requests {client_requests;_ } = 
  Counter.incr client_requests

let tick_append_entries_failure {append_failures; _}  = 
  Counter.incr append_failures

let msg_processing {msg_processing; } = msg_processing

let hb_processing  {hb_processing; } = hb_processing 
