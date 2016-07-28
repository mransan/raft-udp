open Lwt.Infix 
open !Lwt_log_core

module UPb = Raft_udp_pb
module APb = Raft_app_pb
module Pb_util = Raft_udp_pbutil
module Conf = Raft_udp_conf

module U  = Lwt_unix 

module Counter_srv = Raft_app_srv.Make(struct

  type tx_data = Counter_pb.tx 

  let decode bytes = 
    let decoder = Pbrt.Decoder.of_bytes bytes in 
    Counter_pb.decode_tx decoder 

end)

module State = struct

  type t = (int * int) list 
    (* 
     * In the demo app the state is the list of monotically increasing 
     * counter value along with the process id of the client which submitted 
     * the transaction. 
     * 
     * For instance : 
     *  [
     *    (3, 20003);
     *    (2, 20001);
     *    (1, 20002);
     *    (0, 20003);
     *  ]
     *) 


  (** [process state counter_value process_id] check that the [counter_value] 
    * is monotically increasing the [state]. If so the new [state] is returned
    * else [Not_found] is raised
    *) 
  let process t counter_value process_id = 
    match t with
    | [] -> (counter_value, process_id) :: [] 
    | (last_counter_value,  _ )::_ -> 
      if last_counter_value < counter_value 
      then (counter_value, process_id) :: t 
      else raise Not_found
  
  (** [empty] is the initial empty state *)
  let empty = []

end 

let process_demo_app_request logger (validations, notify) state = 
  Lwt_list.fold_left_s (fun (tx_validations, state) tx -> 

    let {
      Counter_srv.tx_id; 
      tx_data = {Counter_pb.counter_value; process_id};
    } = tx in

    match State.process state counter_value process_id with
    | state -> 
      log_f ~logger ~level:Notice "Added: (%06i, %6i) from tx_id: %s" counter_value process_id tx_id
      >|= (fun () -> 
        let tx_validation = Raft_app_srv.({
          tx_id; 
          result = Raft_app_srv.Validation_result_ok; 
        }) in 
        (tx_validation::tx_validations, state) 
      )

    | exception Not_found -> 
      let tx_validation = Raft_app_srv.({
        tx_id; 
        result = Raft_app_srv.Validation_result_error "Not a valid counter value"; 
      }) in 
      Lwt.return (tx_validation::tx_validations, state)

  ) ([], state) validations 

  >|=(fun (tx_validations, state) -> 
    notify @@ List.rev tx_validations; 
    state
  ) 

let main configuration log server_id () = 
  begin 
    let to_file = if log then Some (Printf.sprintf "app%i.log" server_id) else None in 
    Raft_utl_lwt.make_logger ?to_file ()  
  end
  >>=(fun logger -> 

    match Counter_srv.start logger configuration server_id with
    | None -> 
      Lwt.fail_with "Error starting App server" 

    | Some request_stream -> 
      Lwt_stream.fold_s (fun request state -> 
        process_demo_app_request logger request state
      ) request_stream State.empty

      >|= ignore 
  )

let () = 
  let configuration = Conf.default_configuration () in

  let log = ref false in 
  let log_spec = Arg.Set log  in

  let id, id_spec = Raft_udp_conf.get_id_cmdline configuration in
  
  Arg.parse [
    ("--log", log_spec, " : enable logging");
    ("--id", id_spec, " : raft server id");
  ] (fun _ -> ()) "test.ml";

  Sys.set_signal Sys.sigpipe Sys.Signal_ignore ; 
  Lwt_main.run (main configuration !log !id ())
