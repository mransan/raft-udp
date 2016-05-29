open Lwt.Infix 
open Lwt_log_core

module RPb = Raft_pb
module Pb  = Raft_udp_pb

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

let make logger {Pb.log_record_directory; _ }  = 
  let filename = Filename.concat log_record_directory "record.data" in 
  Lwt_io.open_file Lwt_io.output filename  
  >>=(fun file -> 
    log_f ~logger ~level:Notice "[Log Record] creating log record file: %s\n" filename
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
      log_f ~logger ~level:Notice "[Log Record] log_entry appended (index: %10i, id: %s)" 
        index id)
  ) log_entries
  >>=(fun () ->
   Lwt_io.flush handle
  ) 
