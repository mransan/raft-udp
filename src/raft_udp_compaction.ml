open Lwt.Infix 

module RState = Raft_state 
module RPb    = Raft_pb
module RRev_log_cache = Raft_revlogcache

let perform_compaction state =

  let {
    RPb.to_be_expanded;
    RPb.to_be_compacted;
  } = RState.compaction state in 

  Lwt_list.fold_left_s (fun compacter_intervals log_interval -> 
    let encoder = Pbrt.Encoder.create () in 
    RPb.encode_log_interval log_interval encoder; 
    let filename = 
      Printf.sprintf "log_interval_%i_%i.data" 
        state.RPb.id
        log_interval.RPb.prev_index
    in  

    Lwt_io.open_file Lwt_io.output filename  
    >>=(fun file ->
      let bytes = Pbrt.Encoder.to_bytes encoder in 
      Lwt_io.write_from_exactly file bytes 0 (Bytes.length bytes) 
      >>=(fun () -> Lwt_io.close file)
    )
    >|=(fun () -> 
      {log_interval with 
       RPb.rev_log_entries = RPb.Compacted {RPb.record_id = filename} }::compacter_intervals
    )
  ) [] to_be_compacted

let update_state compacter_intervalss state = 
  let global_cache = List.fold_left (fun global_cache log_interval ->
      RRev_log_cache.replace log_interval global_cache  
    ) state.RPb.global_cache compacter_intervalss
  in
  {state with RPb.global_cache}
