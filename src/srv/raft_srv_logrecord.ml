open Lwt.Infix 
open !Lwt_log_core

module Conf = Raft_com_conf

module Rocks = Raft_rocks

let section = Section.make (Printf.sprintf "%10s" "LogRecord")

type t = Rocks.db 

let dirname configuration server_id  = 
  let {
    Conf.disk_backup = {Conf.log_record_directory; _}; _ 
  } = configuration in 
  let dirname = Printf.sprintf "raft_%03i.data" server_id in
  Filename.concat log_record_directory dirname

let make configuration server_id = 
  let dirname = dirname configuration  server_id in 
  let db = Rocks.init dirname in 
  log_f ~level:Notice ~section
        "Creating log record file: %s\n" dirname
  >|= (fun () -> db)

let iter_log_entries log_entries f = 
  
  let rec aux from prev ranges f = function 
    | [] -> (from, prev)::ranges
    | ({Raft_log.index;_} as log) :: tl -> begin  
      f log; 
      if from = -1 
      then aux index index [] f tl  
      else 
        if index = (prev + 1)
        then aux from index ranges f tl  
        else aux index index ((from, prev)::ranges) f tl
    end 
  in

  let ranges = aux (-1) (-1) [] f log_entries in 

  String.concat ", " @@ List.rev_map (fun (from, to_) -> 
    Printf.sprintf "[%i - %i]" from to_
  ) ranges

let add_logs log_entries db = 
  let ranges = iter_log_entries log_entries (fun log ->
    Raft_rocks.add_log ~log ~committed:false ~db ()
  ) in 

  log_f ~level:Notice ~section "Log entries added %s" ranges 
    
let set_committed log_entries db = 
  let ranges = iter_log_entries log_entries (fun log ->
    Raft_rocks.set_committed_by_index ~index:log.Raft_log.index ~db ()
  ) in 

  log_f ~level:Notice ~section "Log entries committed %s" ranges 

let delete_logs log_entries db = 
  let ranges = iter_log_entries log_entries (fun log ->
    Raft_rocks.delete_by_index ~index:log.Raft_log.index ~db ()
  ) in 

  log_f ~level:Notice ~section "Log entries deleted %s" ranges 

let read_log_records db f e0 = 
  let rec aux count acc = function
    | Rocks.End -> Lwt.return acc 
    | Rocks.Value ((log, is_commited, _), k) -> 
      let acc = f acc log is_commited in 
      if count mod 1_000 = 0
      then
        Lwt_unix.yield () 
        >>=(fun () -> aux (count + 1) acc (k ())) 
      else 
        aux (count + 1) acc (k ())
  in 
  aux 0 e0 (Rocks.forward_by_index ~db ())

