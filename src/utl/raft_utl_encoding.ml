let byte pos bytes = 
  int_of_char (Bytes.unsafe_get bytes pos)

exception Overflow

let check_overflow pos bytes size : unit = 
  if Bytes.length bytes < pos + size 
  then raise Overflow
  else () 

module Int32LE  = struct

  type t  = int32 

  let size = 4
  
  let unsafe_decode pos bytes =
    let b1 = byte (pos + 0) bytes in
    let b2 = byte (pos + 1) bytes in
    let b3 = byte (pos + 2) bytes in
    let b4 = byte (pos + 3) bytes in
    Int32.(add (shift_left (of_int b4) 24)
           (add (shift_left (of_int b3) 16)
            (add (shift_left (of_int b2) 8)
             (of_int b1))))

  let unsafe_encode pos bytes i = 
    Bytes.unsafe_set bytes (pos + 0) (char_of_int Int32.(to_int (logand 0xffl i)));
    Bytes.unsafe_set bytes (pos + 1) (char_of_int Int32.(to_int (logand 0xffl (shift_right i 8))));
    Bytes.unsafe_set bytes (pos + 2) (char_of_int Int32.(to_int (logand 0xffl (shift_right i 16))));
    Bytes.unsafe_set bytes (pos + 3) (char_of_int Int32.(to_int (logand 0xffl (shift_right i 24))))

  let decode pos bytes =
    check_overflow pos bytes size; 
    unsafe_decode pos bytes

  let encode pos bytes i = 
    check_overflow pos bytes size; 
    unsafe_encode pos bytes i

end 
