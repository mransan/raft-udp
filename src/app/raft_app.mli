



module type App_sig  = sig 

  type tx 

  val encode : tx -> Pbrt.Encoder.t -> unit 

  val decode : Pbrt.Decoder.t -> tx  

  type validation = 
    | Ok 
    | Error of string 

  val validate : tx -> validation 

end

module Make(App:App_sig) : sig 

  val start : Raft_udp_pb.configuration -> Lwt_log_core.logger -> unit Lwt.t  

end 
