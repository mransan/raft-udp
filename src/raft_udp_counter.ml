


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
