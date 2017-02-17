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

let append_committed_data logger log_entries db = 
  Lwt_list.iter_s (fun ({Raft_log.index; id; _} as log) -> 
    Rocks.add_log ~log ~committed:true ~db ();
    log_f ~logger ~level:Notice ~section 
          "log_entry appended (index: %10i, id: %s)" 
          index id
  ) log_entries

let read_log_records db f e0 = 
  let rec aux count acc = function
    | Rocks.End -> Lwt.return acc 
    | Rocks.Value ((log, _, _), k) -> 
      let acc = f acc log in 
      if count mod 1_000 = 0
      then
        Lwt_unix.yield () 
        >>=(fun () -> aux (count + 1) acc (k ())) 
      else 
        aux (count + 1) acc (k ())
  in 
  aux 0 e0 (Rocks.forward_by_index ~db ())
