open Lwt.Infix 

let is_log_file basename filename : bool = 
  let basename_len = String.length basename in 
  let filename_len = String.length filename in 

  let rfc3339_len = 25 in 
  let log_extension_len = 4 in 
  let underscore_len = 1 in 
  
  let expected_len = 
    basename_len + rfc3339_len + log_extension_len + underscore_len 
  in 

  if filename_len <> expected_len
  then false
  else 
    if String.sub filename 0 (String.length basename) <> basename
    then 
      false
    else 
      let ptime_str = 
        String.sub filename (basename_len + underscore_len) rfc3339_len 
      in 
      match Ptime.of_rfc3339 ptime_str with
      | Ok _ -> true 
      | Error _ -> false 

let all_log_files basename = 
  Lwt_stream.fold (fun filename l -> 
    if is_log_file basename filename
    then begin 
      Printf.printf "prev log file: %s\n%!" filename;
      filename::l
    end 
    else l  
  ) (Lwt_unix.files_of_directory (Sys.getcwd ())) [] 

let delete_old_log_files keep_n basename = 
  all_log_files basename
  >>=(fun files -> 
    let files = 
      let cmp (x:string) (y:string) = compare x y in 
      List.sort cmp files 
    in 
    if List.length files <= keep_n
    then Lwt.return_unit 
    else 
      let rec aux l = function 
        | 0 -> Lwt.return_unit
        | n -> 
          Lwt_unix.unlink (List.hd l)
          >>=(fun () -> aux (List.tl l) (n - 1))
      in 
      aux files (List.length files - keep_n) 
  )  
           
let start ~basename  ~interval () = 

  let template  = 
    "$(date).$(milliseconds) [$(level)] [$(section)] : $(message)" 
  in

  let rec aux () = 
    let timestamp = Ptime.to_rfc3339 (Ptime_clock.now ()) in 
    let file_name = Printf.sprintf "%s_%s.log" basename timestamp in
    Lwt_log.file ~mode:`Append ~template ~file_name ()
    >>= (fun logger -> 
      Lwt_log_core.default := logger; 
      delete_old_log_files 3 basename
      >>=(fun () -> Lwt_unix.sleep (float_of_int interval)) 
      >>= aux 
    )
  in 
  aux () 
