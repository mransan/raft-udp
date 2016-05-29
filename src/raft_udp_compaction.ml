open Lwt.Infix 
open Lwt_log_core

module RState = Raft_state 
module RPb    = Raft_pb
module RRev_log_cache = Raft_revlogcache

let compaction_filename ~server_id ~prev_index () = 
  Printf.sprintf "log_interval_%03i_%012i.data" 
    server_id 
    prev_index

let compact logger server_id to_be_compacted =  
  
  Lwt_list.fold_left_s (fun modified_intervals log_interval -> 
    let encoder = Pbrt.Encoder.create () in 
    RPb.encode_log_interval log_interval encoder; 
    let filename = compaction_filename 
      ~server_id
      ~prev_index:log_interval.RPb.prev_index 
      ()
    in  

    log_f ~logger ~level:Notice "[Compaction] Compacting to file: %s" filename 
    >>=(fun () -> 
      Lwt_io.open_file Lwt_io.output filename  
    )
    >>=(fun file ->
      let bytes = Pbrt.Encoder.to_bytes encoder in 
      Lwt_io.write_from_exactly file bytes 0 (Bytes.length bytes) 
      >>=(fun () -> Lwt_io.close file)
    )
    >|=(fun () -> 
      {log_interval with 
       RPb.rev_log_entries = RPb.Compacted {RPb.record_id = filename} }::modified_intervals 
    )
  ) [] to_be_compacted

let read_from_file ~logger ~server_id ~prev_index () = 
  let filename = compaction_filename ~server_id ~prev_index () in  
  log_f ~logger ~level:Notice "[Compaction] De-Compacting from file: %s" filename 
  >>=(fun () -> Lwt_io.open_file Lwt_io.input filename) 
  >>=(fun file ->
    Lwt_io.length file
    >|= (fun file_len -> (file, file_len))
  )
  >>=(fun (file, file_len) -> 
    let bytes_len = Int64.to_int file_len in 
    let bytes = Bytes.create bytes_len in 
    Lwt_io.read_into_exactly file bytes 0 bytes_len 
    >>=(fun () -> 
      Lwt_io.close file
    )
    >|=(fun () -> 
      let decoder = Pbrt.Decoder.of_bytes bytes in 
      (RPb.decode_log_interval decoder, filename) 
    )
  )

let expand logger server_id to_be_expanded = 
  Lwt_list.fold_left_s (fun modified_intervals log_interval -> 

    read_from_file ~logger ~server_id ~prev_index:log_interval.RPb.prev_index ()
    >|=(fun (file_interval, _ ) -> 
      assert(file_interval.RPb.prev_index = log_interval.RPb.prev_index);
      assert(file_interval.RPb.prev_term = log_interval.RPb.prev_term);
      assert(file_interval.RPb.last_index = log_interval.RPb.last_index);
      file_interval::modified_intervals
    ) 
  ) [] to_be_expanded

let perform_compaction logger state =

  let {
    RPb.to_be_expanded;
    RPb.to_be_compacted;
  } = RState.compaction state in 

  let id = state.RPb.id in 

  compact logger id to_be_compacted
  >>=(fun modified_intervals1 -> 
    expand logger id to_be_expanded
    >|=(fun modified_intervals2 ->  
      modified_intervals1 @ modified_intervals2
    )
  )

let update_state logger modified_intervals state = 
  log_f ~logger ~level:Notice "[Compaction] Updating state with %i modified intervals"
    (List.length modified_intervals)
  >|=(fun () ->
    let global_cache = List.fold_left (fun global_cache log_interval ->
        RRev_log_cache.replace log_interval global_cache  
      ) state.RPb.global_cache modified_intervals
    in
    {state with RPb.global_cache}
  ) 

let load_previous_log_intervals ~logger ~server_id () = 

  let rec aux log_intervals prev_index = 
    Lwt.catch (fun () -> 
      read_from_file ~logger ~server_id ~prev_index ()
      >>=(fun (({RPb.last_index; rev_log_entries; _}  as log_interval), filename) ->

        let log_interval = match rev_log_entries with
          | RPb.Compacted _ -> log_interval
          | RPb.Expanded  _ -> {log_interval with 
            RPb.rev_log_entries = RPb.Compacted {RPb.record_id = filename}
          } 
        in

        aux (log_interval::log_intervals) last_index 
      )
    ) (* with *) (fun exn -> 
      Lwt.return log_intervals
    )
  in 
  aux [] 0 


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

type handle = Lwt_io.output_channel  

let initiate _ = 
  (* TODO add filename in configuration
   *)
  let filename = "record.data" in 
  Lwt_io.open_file Lwt_io.output filename  

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

let append_commited_data log_entries handle = 

  let size_bytes = Bytes.create 4 in 
  Lwt_list.iter_s (fun log_entry -> 
    append size_bytes log_entry handle
  ) log_entries
  >>=(fun () ->
   Lwt_io.flush handle
  ) 
