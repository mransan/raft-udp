

module Counter = struct 

  type t = {
    mutable t0 : float;
    mutable t0_counter : int;
    mutable now_counter : int; 
  }
  
  let make ?initial_counter:(t0_counter = 0) () = {
    t0 = Unix.gettimeofday(); 
    t0_counter;
    now_counter = t0_counter;
  }
  
  let incr t = 
    t.now_counter <- t.now_counter + 1 
  
  let set t now_counter = 
    t.now_counter <- now_counter
  
  let rate t = 
    let t0 = Unix.gettimeofday () in
    let rate = 
      let nom = float_of_int (t.now_counter - t.t0_counter) in 
      let den = t0 -. t.t0 in 
      nom /. den
    in 
    t.t0 <- t0;
    t.t0_counter <- t.now_counter; 
    rate
  
  let value {now_counter; _ } = now_counter

end

module Perf = struct

  type t = {
    mutable avg : float;
    mutable count : int;
    mutable min : float;
    mutable max : float;
  } 

  let make () = {
    avg = 0.;
    count = 0;
    min = 0.;
    max = 0.;
  }

  let reset t = 
    t.avg   <- 0.;
    t.count <- 0;
    t.min   <- 0.;
    t.max   <- 0.;
    ()

  let add t v = 
    t.count <- t.count + 1; 
    t.avg <- t.avg +. ( (v -. t.avg) /. (float_of_int t.count)); 
    t.min <- (min t.min v); 
    t.max <- (max t.max v)
  
  let f1 t f a1 = 
    let t0 = Unix.gettimeofday () in 
    let r  = f a1 in 
    add t (Unix.gettimeofday () -. t0); 
    r 
  
  let f2 t f a1 a2 = 
    let t0 = Unix.gettimeofday () in 
    let r  = f a1 a2 in 
    add t (Unix.gettimeofday () -. t0); 
    r 
  
  let f3 t f a1 a2 a3 = 
    let t0 = Unix.gettimeofday () in 
    let r  = f a1 a2 a3 in 
    add t (Unix.gettimeofday () -. t0); 
    r 

  let stats ?reset:do_reset t = 
    let r = (t.min, t.max, t.avg, t.count) in 
    begin match do_reset with
    | None -> ()
    | Some () -> reset t
    end;
    r

  let avg ?reset:do_reset ?unit_ ({avg; _ } as t)  = 
    let r = match unit_ with
      | None -> avg 
      | Some `Ms -> avg *. 1_000. 
      | Some `Us -> avg *. 1_000_000.  
    in
    begin match do_reset with
    | None -> ()
    | Some () -> reset t
    end;
    r
end
