module Pb = Raft_udp_pb 

type send_app_request_f  = Raft_udp_pb.app_request option -> unit 

type t = send_app_request_f * Raft_udp_pb.app_response Lwt_stream.t 


let handle_app_request = function  
  | Pb.Validate_log {Pb.request_id; data; } -> 
    if Bytes.length data > 10 
    then Pb.({
      request_id; 
      result = Validate_failure {
        error_message = "Data is too big"; 
        error_code = 1;
      }
    }) 
    else Pb.({
      request_id; 
      result = Validate_success
    }) 
  | Pb.Commit_log {Pb.request_id; _ } -> Pb.({
    request_id; 
    result  = Commit_log_ack
  })

let make logger configuration stats = 

  let (
    request_stream, 
    push_request_f
  ) = Lwt_stream.create() in 

  let (
    response_stream, 
    push_response_f, 
    set_response_ref
  ) = Lwt_stream.create_with_reference () in 


  let request_stream : unit Lwt.t = Lwt_stream.iter (fun request -> 
    let response = handle_app_request request in 
    push_response_f (Some response)
  ) request_stream in 

  set_response_ref request_stream; 

  (push_request_f, response_stream)
