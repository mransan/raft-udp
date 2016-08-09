open Lwt.Infix

let write_message_header = 
  let buffer = Bytes.create Raft_utl_encoding.Int32LE.size in 

  fun ~message_size fd () ->
    Raft_utl_encoding.Int32LE.unsafe_encode 0 buffer (Int32.of_int message_size);
    Lwt_unix.write fd buffer 0 Raft_utl_encoding.Int32LE.size 
    >>=(function
      | 0 -> 
        Lwt.fail_with "Connection closed by App server (write size = 0)"
      | n when n = Raft_utl_encoding.Int32LE.size ->
        Lwt.return_unit 
      | n -> 
        Lwt.fail_with @@ 
          Printf.sprintf 
          "Invalid write size of message header, expected: %i, got: %i"
          Raft_utl_encoding.Int32LE.size n  
    )

type header = {message_size : int}

let message_size {message_size;} = message_size 

let read_message_header = 
  let buffer = Bytes.create Raft_utl_encoding.Int32LE.size in 
  (* Fixed size buffer for the message header needs to be created once
   *)

  fun fd -> 
    Lwt_unix.read fd buffer 0 Raft_utl_encoding.Int32LE.size  
    >>=(function
      | 0 -> 
        Lwt.fail_with "Connection closed by client (read size = 0)" 

      | n when n = Raft_utl_encoding.Int32LE.size -> 
        (* TODO assert the message size is not greated than the largest
         * [int] value 
         *) 
        let message_size = 
            Int32.to_int @@ Raft_utl_encoding.Int32LE.unsafe_decode 0 buffer
        in 
        Lwt.return {message_size}

      | n -> 
        Lwt.fail_with @@ 
          Printf.sprintf 
          "Invalid message header size, expected: %i, got: %i" 
          Raft_utl_encoding.Int32LE.size n
    )


