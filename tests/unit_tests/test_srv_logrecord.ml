open Lwt.Infix 
module LR = Raft_srv_logrecord 

module Conf = Raft_udp_conf 
module UPb = Raft_udp_pb 

let logger = Lwt_log_core.null 

let get_tmp_dir () = 
  let name = Filename.temp_file "test_srv_logrecord" ".data" in 
  Sys.remove name; 
  Unix.mkdir name 0o770; 
  name
  
let main = 

  let dirname = get_tmp_dir () in 

  Printf.printf "Using tmp data directory %s for test log record.\n" dirname;
    
  let configuration = 
    (* 
     * Default configuration + temporary directory for 
     * the [log_record_directory] to ensure no data is affected 
     * by running the test
     *)
    let configuration = Conf.default_configuration () in 
    let disk_backup = configuration.UPb.disk_backup in 
    let disk_backup = {disk_backup with
      UPb.log_record_directory = dirname;
    } in 
    {configuration with UPb.disk_backup}
  in 
  
  let server_id = 0 in 

  let test_log_entry1 = Raft_pb.({
    index = 10; 
    term = 3; 
    data = Bytes.of_string "This is a test data";
    id = "App id";
  }) in 
  
  let test_log_entry2 = Raft_pb.({
    index = 21; 
    term = 10; 
    data = Bytes.of_string "This is a test data again";
    id = "Another App id";
  }) in 
  
  let test_log_entry3 = Raft_pb.({
    index = 21; 
    term = 10; 
    data = Bytes.of_string "This is a test data again";
    id = "Another App id";
  }) in 
  
  LR.make ~logger configuration server_id

  >>= Raft_utl_lwt.tap (fun handle ->
    (* 
     * We use a list of 2 to make sure the ordering of log entries is respected
     * ie they will be saved in chronological order (test_log_entry1 then 
     * test_log_entry2) 
     *)
    LR.append_commited_data 
      ~logger
      ~rev_log_entries:[test_log_entry1;test_log_entry2] 
      handle
  ) 

  >>= (fun handle -> 
    (* 
     * Close and re-open then log record handle 
     *)
    LR.close handle
    >>= (fun () -> 
      LR.make ~logger configuration server_id
    ) 
  ) 

  >>= Raft_utl_lwt.tap (fun handle ->
    (* 
     * This make sure that the log is correctly appended even after 
     * the handle was closed and re-opened. 
     *)
    LR.append_commited_data 
      ~logger
      ~rev_log_entries:[test_log_entry3] 
      handle
  ) 
  
  >>= (fun _ -> 
    let acc l log_entry = 
      log_entry::l
    in 
    let e0 = [] in 
    LR.read_log_records ~logger configuration server_id acc e0 
  ) 

  >|=(fun log_entries ->
    assert(log_entries = [ test_log_entry3; test_log_entry2; test_log_entry1; ])
  )  

  >|=(fun () ->
    (* 
     * Only cleanup resources on success so that failure could be investigated. 
     *
     * Since the data directory is in the tmp one of the system it should not
     * affect diskspace since it can be deleted safely.
     *)
    begin match Sys.command @@ Printf.sprintf "rm -r %s" dirname with
    | 0 -> () 
    | rcode -> 
      Printf.eprintf 
        "Error removing tmp directory: %s, rcode: %i\n" 
        dirname rcode 
    end; 
    Printf.printf "Success...\n"
  )

let () =
  Lwt_main.run main  
