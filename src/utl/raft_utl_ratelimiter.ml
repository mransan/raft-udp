open Lwt.Infix


let wrap rate stream = 
  let events = Array.make rate (-. 1.) in 
  let t0 = Mtime.counter () in 
  let i = ref 0 in 

  let rate, min_delta = 
    let precision = 5 in
    if rate <= precision
    then rate, 1.
    else (rate / precision), (1. /. (float_of_int precision))
  in

  let set_events ~from ~len = 
    let time = Mtime.(count t0 |> to_s) in 
    let next = from + len in
    if next > rate
    then begin
      Array.fill events from (rate - from) time; 
      let next = next - rate in 
      Array.fill events 0 next time;
      next
    end
    else begin
      Array.fill events from len time;
      next
    end
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

        let delta = time -. events.((!i + len - 1) mod rate) in 
        if delta < min_delta
        then 
          Lwt_unix.sleep (min_delta -. delta) 
          >|= (fun () ->  
            i := set_events ~from:!i ~len;
            l
          )
        else begin 
          i := set_events ~from:!i ~len;
          Lwt.return l 
        end 
    )
