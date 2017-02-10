open Lwt.Infix 
open !Lwt_log_core

module RState = Raft_state 
module RLog = Raft_log

module Pb = Raft_udp_pb

let section = Section.make (Printf.sprintf "%10s" "Compaction")

let compaction_filename ~server_id ~prev_index ~configuration () = 
  let filename = Printf.sprintf "log_interval_%03i_%012i.data" 
    server_id 
    prev_index
  in
  Filename.concat configuration.Pb.disk_backup.Pb.compaction_directory filename

let srv_log_interval_of_raft_log_interval (i:RLog.log_interval)  = 
  (* TODO FIXXXXXXXXXXX *)
  let i:Raft_srv_pb.log_interval = Obj.magic i in 
  i

let raft_log_interval_of_srv_log_interval (i:Raft_srv_pb.log_interval)  = 
  (* TODO FIXXXXXXXXXXX *)
  let i:RLog.log_interval = Obj.magic i in 
  i

(* 
 * Performs a compaction by storing the [log_interval] on disk
 * and return the compacted version of it. 
 * (ie log_entry.Raft_pb.rev_log_entries is set to [Compacted].
 *
 *)
let compact logger server_id configuration to_be_compacted =  
  
  Lwt_list.fold_left_s (fun modified_intervals log_interval -> 
    let encoder = Pbrt.Encoder.create () in 
    Raft_srv_pb.encode_log_interval 
      (srv_log_interval_of_raft_log_interval log_interval) encoder; 
    let filename = compaction_filename 
      ~server_id
      ~prev_index:log_interval.RLog.prev_index 
      ~configuration
      ()
    in  

    log_f ~logger ~level:Notice ~section "Compacting to file: %s" filename 
    >>=(fun () -> 
      Lwt_io.open_file ~mode:Lwt_io.output filename  
    )
    >>=(fun file ->
      let bytes = Pbrt.Encoder.to_bytes encoder in 
      Lwt_io.write_from_exactly file bytes 0 (Bytes.length bytes) 
      >>=(fun () -> Lwt_io.close file)
    )
    >|=(fun () -> 
      RLog.({log_interval with 
       rev_log_entries = Compacted {record_id = filename} })::modified_intervals 
    )
  ) [] to_be_compacted

let read_from_file ~logger ~server_id ~configuration ~prev_index () = 
  let filename = compaction_filename ~server_id ~prev_index ~configuration () in  
  log_f ~logger ~level:Notice ~section "De-Compacting from file: %s" filename 
  >>=(fun () -> Lwt_io.open_file ~mode:Lwt_io.input filename) 
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
      let log_interval = 
        Raft_srv_pb.decode_log_interval decoder
        |> raft_log_interval_of_srv_log_interval
      in 

      (log_interval, filename) 
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
      ~logger ~server_id ~prev_index:log_interval.RLog.prev_index ~configuration ()
    >|=(fun (file_interval, _ ) -> 
      assert(file_interval.RLog.prev_index = log_interval.RLog.prev_index);
      assert(file_interval.RLog.prev_term = log_interval.RLog.prev_term);
      assert(file_interval.RLog.last_index = log_interval.RLog.last_index);
      file_interval::modified_intervals
    ) 
  ) [] to_be_expanded

let perform_compaction logger configuration state =

  let {
    RState.to_be_expanded;
    RState.to_be_compacted;
  } = RState.compaction state in 

  let id = state.RState.id in 

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
    let log = List.fold_left (fun global_cache log_interval ->
        RLog.Past_entries.replace log_interval global_cache  
      ) state.RState.log modified_intervals
    in
    {state with RState.log}
  ) 

let load_previous_log_intervals logger configuration server_id = 

  let rec aux log_intervals prev_index = 
    Lwt.catch (fun () -> 
      read_from_file ~logger ~server_id ~prev_index ~configuration ()
      >>=(fun (({RLog.last_index; rev_log_entries; _}  as log_interval), filename) ->

        let log_interval = match rev_log_entries with
          | RLog.Compacted _ -> log_interval
          | RLog.Expanded  _ -> {log_interval with 
            RLog.rev_log_entries = RLog.Compacted {RLog.record_id = filename}
          } 
        in

        aux (log_interval::log_intervals) last_index 
      )
    ) (* with *) (fun _ -> 
      (* TODO: be more precise that the exception should really be the one 
       * not finding the next log interval file
       *)
      Lwt.return log_intervals
    )
  in 
  aux [] 0 


