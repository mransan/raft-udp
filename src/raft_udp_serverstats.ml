module Counter = Raft_udp_counter.Counter 
module Perf    = Raft_udp_counter.Perf

open Lwt.Infix 

type t =  {
  raft_msg_recv : Counter.t; 
  raft_msg_send : Counter.t; 
  log_count     : Counter.t; 
  heartbeat      : Counter.t; 
  append_failures: Counter.t;
  new_client_connection : Counter.t;
  msg_processing: Perf.t;
  hb_processing : Perf.t; 
  mutable print_stats_t : unit Lwt.t;
}

let print_stats_t t =

  let {
    raft_msg_recv;
    raft_msg_send;
    log_count;
    heartbeat;
    new_client_connection;
    msg_processing;
    append_failures;
    hb_processing; _
  } = t in
  
  let print_header_every = 20 in 

  let rec aux counter () =
    Lwt_unix.sleep 1.
    >>=(fun () -> 
      if counter = 0 
      then 
        Lwt_io.printf 
          " %15s | %15s | %15s | %10s | %8s | %4s | %6s | %12s | %12s | \n" 
          "clt conn/s"
          "recv msg/s"
          "sent msg/s" 
          "log/s"
          "log nb"
          "hb/s"
          "er/s"
          "avg msg (us)"
          "avg hb (us)"
        >|=(fun () -> print_header_every)
      else
        Lwt.return counter 
    )
    >>=(fun counter ->
      Lwt_io.printf " %15.3f | %15.3f | %15.3f | %10.3f | %8i | %4.1f | %6.1f | %12.3f | %12.3f | \n"
        (Counter.rate new_client_connection)
        (Counter.rate raft_msg_recv)
        (Counter.rate raft_msg_send)
        (Counter.rate log_count)
        (Counter.value log_count)
        (Counter.rate heartbeat)
        (Counter.rate append_failures)
        (Perf.avg ~reset:() ~unit_:`Us msg_processing)
        (Perf.avg ~reset:() ~unit_:`Us hb_processing)
      >>=(fun () -> aux (counter - 1) ()) 
    )
  in
  aux 0 () 

let make ~initial_log_size () = 
  let t = {
    raft_msg_send = Counter.make ();
    raft_msg_recv = Counter.make (); 
    log_count     = Counter.make ~initial_counter:initial_log_size ();
    heartbeat      = Counter.make ();
    new_client_connection = Counter.make ();
    msg_processing= Perf.make (); 
    hb_processing = Perf.make (); 
    append_failures = Counter.make (); 
    print_stats_t = Lwt.return_unit;
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

let tick_append_entries_failure {append_failures; _}  = 
  Counter.incr append_failures

let msg_processing {msg_processing; } = msg_processing

let hb_processing  {hb_processing; } = hb_processing 
