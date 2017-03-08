open Lwt.Infix 
open !Lwt_log_core

module Conf = Raft_com_conf
module Rocks = Raft_rocks

module RTypes = Raft_types
module RLog = Raft_log
module RProtocol = Raft_protocol

let section = Section.make (Printf.sprintf "%10s" "LogRecord")

type t = {
  rocksdb_db : Rocks.db;
  configuration : Conf.t; 
  server_id : int;
}

let dirname configuration server_id  = 
  let {Conf.storage_directory; _ } = configuration in 
  let dirname = Printf.sprintf "raft_%03i.data" server_id in
  Filename.concat storage_directory dirname

let make configuration server_id = 
  let dirname = dirname configuration  server_id in 
  let rocksdb_db = Rocks.init dirname in 
  log_f ~level:Notice ~section
        "Creating log record file: %s\n" dirname
  >|= (fun () -> {rocksdb_db; configuration; server_id})

let iter_log_entries log_entries f = 
  
  let rec aux from prev ranges f = function 
    | [] -> (from, prev)::ranges
    | ({RLog.index;_} as log) :: tl -> begin  
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

let add_logs log_entries {rocksdb_db; _} = 
  let ranges = iter_log_entries log_entries (fun log ->
    Raft_rocks.add_log ~log ~committed:false ~db:rocksdb_db ()
  ) in 

  log_f ~level:Notice ~section "Log entries added %s" ranges 
    
let set_committed log_entries {rocksdb_db; _} = 
  let ranges = iter_log_entries log_entries (fun log ->
    Raft_rocks.set_committed_by_index 
                          ~index:log.RLog.index ~db:rocksdb_db ()
  ) in 

  log_f ~level:Notice ~section "Log entries committed %s" ranges 

let delete_logs log_entries {rocksdb_db; _} = 
  let ranges = iter_log_entries log_entries (fun log ->
    Raft_rocks.delete_by_index ~index:log.RLog.index ~db:rocksdb_db ()
  ) in 

  log_f ~level:Notice ~section "Log entries deleted %s" ranges 

let read_log_records max_record {rocksdb_db; _} f e0 = 
  let rec aux count acc = function
    | Rocks.End -> Lwt.return acc 
    | Rocks.Value ((log, is_commited, _), k) -> 
      let acc = f acc log is_commited in 
      if count = max_record 
      then Lwt.return acc
      else 
        if count mod 1_000 = 0
        then
          Lwt_unix.yield () 
          >>=(fun () -> aux (count + 1) acc (k ())) 
        else 
          aux (count + 1) acc (k ())
  in 
  aux 0 e0 (Rocks.backward_by_index ~db:rocksdb_db ())

let read_raft_state ~now ({configuration; server_id; _} as t) = 

  let {Conf.raft_configuration; _} = configuration in 
  let {RTypes.max_log_size; _} = raft_configuration in 
  let {RLog.lower_bound; _} = max_log_size in 

  (* fold-function which keeps building the log data structure 
   * and record the latest commit log (ie the commit index) *)
  let f acc log_entry is_committed = 
    let b, commit_index = acc in 
    let b = RLog.Builder.add_log_entry b log_entry in 
    let commit_index = 
      if is_committed && log_entry.RLog.index > commit_index
      then  log_entry.RLog.index
      else commit_index 
    in 
    (b, commit_index)
  in  

  (* initial value for the fold over log record *)
  let e0 = (RLog.Builder.make max_log_size, 0 (*commit_index*)) in 

  read_log_records lower_bound t f e0 
  >|=(fun (b, commit_index) -> 
    let log = RLog.Builder.to_log b in 
    let _, current_term = RLog.last_log_index_and_term log in 

    let configuration = raft_configuration in 
    RProtocol.init ~log ~commit_index ~current_term 
                   ~configuration ~now  ~server_id ()
  ) 
