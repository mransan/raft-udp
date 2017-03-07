module Counter = Raft_utl_counter.Counter 
module Perf    = Raft_utl_counter.Perf

open Lwt.Infix 

type server_role = 
  | Leader
  | Follower
  | Candidate 

let string_of_server_role = function
  | None -> " "
  | Some Leader -> "L"
  | Some Follower -> "F"
  | Some Candidate -> "C"

let server_id  = ref 0 

let raft_msg_recv = Counter.make () 

let raft_msg_send = Counter.make () 

let log = Counter.make () 

let heartbeat = Counter.make () 

let append_entries_failure = Counter.make () 

let client_connections = Counter.make () 

let client_requests = Counter.make () 

let msg_processing = Perf.make () 

let hb_processing = Perf.make ()

let add_log_processing = Perf.make () 

let print_header = ref false 

let server_role = ref None

let _ =
  
  let print_header_every = 7 in 

  let rec aux counter () =
    Lwt_unix.sleep 1.
    >>=(fun () -> 
      if counter = 0 && !print_header 
      then 
        Lwt_io.printf 
          "[id   ] | %7s | %7s | %8s | %8s | %4s | %5s | %6s | %6s | %7s | %7s | %7s \n" 
          "r rcv/s"
          "r snt/s" 
          "log/s"
          "log nb"
          "hb/s"
          "clt/s"
          "req/s"
          "er/s"
          "msg(us)"
          "hb(us)"
          "al(us)"
        >|=(fun () -> print_header_every)
      else
        Lwt.return counter 
    )
    >>=(fun counter ->
      Lwt_io.printf "[%2i, %s] | %7.0f | %7.0f | %8.0f | %8i | %4.1f | %5.1f | %6.0f | %6.1f | %7.1f | %7.1f | %7.1f \n"
        !server_id
        (string_of_server_role !server_role)
        (Counter.rate raft_msg_recv)
        (Counter.rate raft_msg_send)
        (Counter.rate log)
        (Counter.value log)
        (Counter.rate heartbeat)
        (Counter.rate client_connections)
        (Counter.rate client_requests)
        (Counter.rate append_entries_failure)
        (Perf.avg ~reset:() ~unit_:`Us msg_processing)
        (Perf.avg ~reset:() ~unit_:`Us hb_processing)
        (Perf.avg ~reset:() ~unit_:`Us add_log_processing)
      >>=(fun () -> aux (counter - 1) ()) 
    )
  in
  aux 0 () 
