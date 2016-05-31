open Lwt.Infix 
open Lwt_log_core

module RPb = Raft_pb
module Pb  = Raft_udp_pb

let section = Section.make (Printf.sprintf "%10s" "LogRecord")

module Int32_encoding = struct 
  
  let byte pos bytes = 
    int_of_char (Bytes.get bytes pos)
  
  let decode_int pos bytes =
    let b1 = byte (pos + 0) bytes in
    let b2 = byte (pos + 1) bytes in
    let b3 = byte (pos + 2) bytes in
    let b4 = byte (pos + 3) bytes in
    Int32.(add (shift_left (of_int b4) 24)
           (add (shift_left (of_int b3) 16)
            (add (shift_left (of_int b2) 8)
             (of_int b1))))
    |> Int32.to_int 

  let encode_int pos i bytes = 
    let i = Int32.of_int i in 
    Bytes.set bytes (pos + 0) (char_of_int Int32.(to_int (logand 0xffl i)));
    Bytes.set bytes (pos + 1) (char_of_int Int32.(to_int (logand 0xffl (shift_right i 8))));
    Bytes.set bytes (pos + 2) (char_of_int Int32.(to_int (logand 0xffl (shift_right i 16))));
    Bytes.set bytes (pos + 3) (char_of_int Int32.(to_int (logand 0xffl (shift_right i 24))))

end 

type t = Lwt_io.output_channel  

let filename {Pb.log_record_directory; _} server_id  = 
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

  Int32_encoding.encode_int 0 data_size size_bytes; 

  Lwt_io.write_from_exactly handle size_bytes 0 4 
  >>=(fun () -> 
    Lwt_io.write_from_exactly handle data_bytes 0 data_size 
  )

let append_commited_data logger log_entries handle = 

  let size_bytes = Bytes.create 4 in 
  Lwt_list.iter_s (fun ({RPb.index; id; _} as log_entry) -> 
    append size_bytes log_entry handle
    >>=(fun () -> 
      log_f ~logger ~level:Notice ~section "log_entry appended (index: %10i, id: %s)" 
        index id)
  ) log_entries
  >>=(fun () -> Lwt_io.flush handle) 

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
      let data_len = Int32_encoding.decode_int 0 size_bytes in 
      let data = Bytes.create data_len in 
      Lwt_io.read_into_exactly file data 0 data_len
      >|=(fun () ->
        let decoder = Pbrt.Decoder.of_bytes data in 
        Some (RPb.decode_log_entry decoder)
      )
    ) 
  ) (* with *) (fun exn -> 
    Lwt_io.eprintlf "Error reading log records: %s" (Printexc.to_string exn)
    >>=(fun () -> Lwt_io.close file)
    >|=(fun () -> None) 
  )

let read_log_records configuration server_id f e0 =

  let filename = filename configuration server_id in 
  Lwt.catch (fun () ->
    Lwt_io.open_file Lwt_io.input filename
    >>=(fun file ->
      let bytes = Bytes.create 4 in 
      let rec aux acc = 
        read_log_entry_from_file bytes file 
        >>=(function
          | None -> Lwt.return acc
          | Some x -> aux (f acc x)
        ) 
      in
      aux e0 
    ) 
  ) (* with *) (function
    | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return e0
    | _ -> Lwt.fail_with "Error reading log record file" 
  )
