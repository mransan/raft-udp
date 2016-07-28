open Lwt.Infix 
open !Lwt_log_core

module RPb = Raft_pb
module Pb  = Raft_udp_pb

let section = Section.make (Printf.sprintf "%10s" "LogRecord")

type t = Lwt_io.output_channel  

let filename {Pb.disk_backup = {Pb.log_record_directory; _}; _ } server_id  = 
  Filename.concat log_record_directory (Printf.sprintf "record_%03i.data" server_id)

let make logger configuration server_id = 
  let filename = filename configuration  server_id in 
  Lwt_io.open_file ~flags:[Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND] ~mode:Lwt_io.output filename  
  >>=(fun file -> 
    log_f ~logger ~level:Notice ~section "Creating log record file: %s\n" filename
    >|=(fun () -> file)
  ) 

let append size_bytes log_entry handle = 

  let data_bytes = 
    let encoder = Pbrt.Encoder.create () in 
    RPb.encode_log_entry log_entry encoder; 
    Pbrt.Encoder.to_bytes encoder
  in  

  let data_size = Bytes.length data_bytes in 

  Raft_utl_encoding.Int32LE.encode 0 size_bytes (Int32.of_int data_size); 

  Lwt_io.write_from_exactly handle size_bytes 0 4 
  >>=(fun () -> 
    Lwt_io.write_from_exactly handle data_bytes 0 data_size 
  )

let append_commited_data ~logger ~rev_log_entries handle = 

  let size_bytes = Bytes.create 4 in 
  Lwt_list.iter_s (fun ({RPb.index; id; _} as log_entry) -> 
    append size_bytes log_entry handle
    >>=(fun () -> 
      log_f ~logger ~level:Notice ~section "log_entry appended (index: %10i, id: %s)" 
        index id)
  ) rev_log_entries
  >>=(fun () -> Lwt_io.flush handle) 

type read_result = 
  | Ok of RPb.log_entry
    (** The next log entry could be read *)
  | Done 
    (** Reached the end of the file *)
  | Error of string 
    (** En error occurred (IO or invalid record on disk for instance) *)

(*
 * Read a single [log_entry] record from the file. 
 *
 * return [None] if no [log_entry] could be read. 
 *
 * Note that we have no way to know before invoking [read]
 * that the file does not contain a valid [log_entry]. We 
 * therefore rely on [read] failing to detect the end of the 
 * file. 
 *)
let read_log_entry_from_file size_bytes file = 
  Lwt.catch (fun () -> 
    Lwt_io.read_into_exactly file size_bytes 0 4 
    >>=(fun () ->
      let data_len = Raft_utl_encoding.Int32LE.decode 0 size_bytes |> Int32.to_int in 
      let data = Bytes.create data_len in 
      Lwt_io.read_into_exactly file data 0 data_len
      >|=(fun () ->
        let decoder = Pbrt.Decoder.of_bytes data in 
        Ok (RPb.decode_log_entry decoder)
      )
    ) 
  ) (* with *) (function
    | End_of_file -> 
      Lwt_io.close file >|= (fun () -> Done)  

    | exn -> 
      Lwt_io.eprintlf "Error reading log records: %s" (Printexc.to_string exn)
      >>=(fun () -> Lwt_io.close file)
      >|=(fun () -> Error (Printf.sprintf "Error reading log records, details: %s" (Printexc.to_string exn))) 
  )

let read_log_records configuration server_id f e0 =

  let filename = filename configuration server_id in 
  Lwt.catch (fun () ->
    Lwt_io.open_file ~mode:Lwt_io.input filename
    >>=(fun file ->
      let bytes = Bytes.create 4 in 
      let rec aux acc = 
        read_log_entry_from_file bytes file 
        >>=(function
          | Done    -> Lwt.return acc
          | Ok x    -> aux (f acc x)
          | Error s -> Lwt.fail_with s
        ) 
      in
      aux e0 
    ) 
  ) (* with *) (function
    | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return e0
    | _ -> Lwt.fail_with "Error reading log record file" 
  )
