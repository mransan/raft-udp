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

let make logger configuration server_id = 
  let dirname = dirname configuration  server_id in 
  let db = Rocks.init dirname in 
  log_f ~logger ~level:Notice ~section
        "Creating log record file: %s\n" dirname
  >|= (fun () -> db)

let add_logs logger log_entries db = 
  Lwt_list.iter_s (fun ({Raft_log.index; id; _} as log) -> 
    Rocks.add_log ~log ~committed:false ~db ();
    log_f ~logger ~level:Notice ~section 
          "log_entry added (index: %10i, id: %s)" 
          index id
  ) log_entries

let set_committed logger log_entries db = 
  Lwt_list.iter_s (fun {Raft_log.index; id;  _} -> 
    Rocks.set_committed_by_index ~index ~db ();
    log_f ~logger ~level:Notice ~section 
          "log_entry committed (index: %10i, id: %s)" 
          index id
  ) log_entries

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

let delete_log_record logger  {Raft_log.index; id; _} db = 
  Rocks.delete_by_index ~index ~db (); 
  log_f ~logger ~level:Notice ~section 
        "Log entry deleted, index: %i, id: %s"  index id
