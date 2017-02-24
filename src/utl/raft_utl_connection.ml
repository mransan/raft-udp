let add_length_prefix b = 
  let b_len = Bytes.length b in 
  let header_b = Bytes.create 4 in 
  EndianBytes.BigEndian.set_int32 header_b 0 (Int32.of_int b_len); 
  Bytes.cat header_b b 

let read_msg_with_header ic buffer = 
  let open Lwt.Infix in 
  Lwt_io.read_into_exactly ic buffer 0 4 
  >>=(fun () -> 
    let len = 
      EndianBytes.BigEndian.get_int32 buffer 0 
      |> Int32.to_int 
    in 

    let current_len = Bytes.length buffer in 

    let buffer = 
      if len > current_len 
      then 
        let rec aux current_len = 
          if len > current_len
          then aux (current_len * 2) 
          else current_len
        in 
        Bytes.create (aux (current_len * 2)) 
      else 
        buffer
    in
    Lwt_io.read_into_exactly ic buffer 0 len
    >|= (fun () -> (buffer, len))
  )

let write_msg_with_header fd buffer = 

  let buffer = add_length_prefix buffer in 
  
  let buffer_len = Bytes.length buffer in 

  let rec aux pos = 
    let open Lwt.Infix in 
    let len = buffer_len - pos in 
    Lwt_unix.write fd buffer pos len 
    >>=(function
      | 0 -> Lwt.fail_with "Error: connection closed (read = 0)"
      | n when n = len -> Lwt.return_unit  
      | n -> aux (pos + n) 
    )  
  in 
  aux 0 
