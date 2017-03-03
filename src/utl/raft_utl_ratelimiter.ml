open Lwt.Infix

let wrap rate stream = 
  let events = Array.make rate (-. 1.) in 
  let t0 = Mtime.counter () in 
  let i = ref 0 in 

  let rate, min_delta = 
    let precision = 1 (* TODO: this could be a function of rate *) in 
    if rate <= precision
    then rate, 1.
    else (rate / precision), (1. /. (float_of_int precision))
  in

  fun () -> 
    Lwt_stream.get stream 
    >>= (function 
      | None -> Lwt.return []
      | Some hd -> 
        let tl = Lwt_stream.get_available_up_to (rate - 1) stream in 
        let l = hd::tl in 
        let len = List.length l in 
        let time = Mtime.(count t0 |> to_s) in 

        let i' = !i + len in
        if i' > rate
        then begin
          Array.fill events !i (rate - !i) time; 
          i := i' - rate;
          Array.fill events 0 !i time;
        end
        else begin
          Array.fill events !i len time;
          i := i';
        end;
        let delta = events.(!i - 1) -. events.(!i mod rate) in
        if delta < min_delta
        then Lwt_unix.sleep (min_delta -. delta) >|= (fun () -> l) 
        else Lwt.return l  
    )
