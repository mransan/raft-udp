
let make_logger ?to_file () = 
  match to_file with
  | None -> Lwt.return Lwt_log_core.null 
  | Some file_name -> 
    let template  = "$(date).$(milliseconds) [$(level)] [$(section)] : $(message)" in
    Lwt_log.file ~mode:`Append ~template ~file_name ()

