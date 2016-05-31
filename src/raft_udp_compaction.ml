open Lwt.Infix 
open Lwt_log_core

module RState = Raft_state 
module RPb    = Raft_pb
module RRev_log_cache = Raft_revlogcache

module Pb = Raft_udp_pb

let section = Section.make (Printf.sprintf "%10s" "Compaction")

let compaction_filename ~server_id ~prev_index ~configuration () = 
  let filename = Printf.sprintf "log_interval_%03i_%012i.data" 
    server_id 
    prev_index
  in
  Filename.concat configuration.Pb.compaction_directory filename


(* 
 * Performs a compaction by storing the [log_interval] on disk
 * and return the compacted version of it. 
 * (ie log_entry.Raft_pb.rev_log_entries is set to [Compacted].
 *
 *)
let compact logger server_id configuration to_be_compacted =  
  
  Lwt_list.fold_left_s (fun modified_intervals log_interval -> 
    let encoder = Pbrt.Encoder.create () in 
    RPb.encode_log_interval log_interval encoder; 
    let filename = compaction_filename 
      ~server_id
      ~prev_index:log_interval.RPb.prev_index 
      ~configuration
      ()
    in  

    log_f ~logger ~level:Notice ~section "Compacting to file: %s" filename 
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

let read_from_file ~logger ~server_id ~configuration ~prev_index () = 
  let filename = compaction_filename ~server_id ~prev_index ~configuration () in  
  log_f ~logger ~level:Notice ~section "De-Compacting from file: %s" filename 
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

(* 
 * Performs a de-compaction by reading the data of [log_interval] from disk. 
 * and return the expanded version of it. 
 * (ie log_entry.Raft_pb.rev_log_entries is set to [Expanded].
 *
 *)
let expand logger server_id configuration to_be_expanded = 
  Lwt_list.fold_left_s (fun modified_intervals log_interval -> 

    read_from_file 
      ~logger ~server_id ~prev_index:log_interval.RPb.prev_index ~configuration ()
    >|=(fun (file_interval, _ ) -> 
      assert(file_interval.RPb.prev_index = log_interval.RPb.prev_index);
      assert(file_interval.RPb.prev_term = log_interval.RPb.prev_term);
      assert(file_interval.RPb.last_index = log_interval.RPb.last_index);
      file_interval::modified_intervals
    ) 
  ) [] to_be_expanded

let perform_compaction logger configuration state =

  let {
    RPb.to_be_expanded;
    RPb.to_be_compacted;
  } = RState.compaction state in 

  let id = state.RPb.id in 

  compact logger id configuration to_be_compacted
  >>=(fun modified_intervals1 -> 
    expand logger id configuration to_be_expanded
    >|=(fun modified_intervals2 ->  
      modified_intervals1 @ modified_intervals2
    )
  )

let update_state logger modified_intervals state = 
  log_f ~logger ~level:Notice ~section "Updating state with %i modified intervals"
    (List.length modified_intervals)
  >|=(fun () ->
    let global_cache = List.fold_left (fun global_cache log_interval ->
        RRev_log_cache.replace log_interval global_cache  
      ) state.RPb.global_cache modified_intervals
    in
    {state with RPb.global_cache}
  ) 

let load_previous_log_intervals logger configuration server_id = 

  let rec aux log_intervals prev_index = 
    Lwt.catch (fun () -> 
      read_from_file ~logger ~server_id ~prev_index ~configuration ()
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


