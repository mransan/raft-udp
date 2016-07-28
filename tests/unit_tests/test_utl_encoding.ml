open Raft_utl_encoding

let test_int32le_i i = 
  let b = Bytes.create Int32LE.size in 
  Int32LE.encode 0 b i; 
  let i'= Int32LE.decode 0 b in 
  assert(i = i')

let () = 
  test_int32le_i 0l;
  test_int32le_i 1l;
  test_int32le_i (-1l);
  test_int32le_i 10l;
  test_int32le_i (-10l);
  test_int32le_i (Int32.max_int);
  test_int32le_i (Int32.min_int)

let () = 
  let b = Bytes.create (Int32LE.size - 1) in 
  match Int32LE.encode 0 b 2l with
  | _ -> assert(false) 
  | exception Overflow -> () 

let () = 
  let b = Bytes.create (Int32LE.size - 1) in 
  match Int32LE.decode 0 b with
  | _ -> assert(false) 
  | exception Overflow -> () 
