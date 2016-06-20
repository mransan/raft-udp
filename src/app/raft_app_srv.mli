
type validation = 
  | Ok 
  | Error of string 

module type App_sig  = sig 

  type tx 

  val decode : bytes -> tx  

  val validate : tx -> validation 

end

module Make(App:App_sig) : sig 

  type validations = App.tx list * (validation list -> unit) 

  val start : Lwt_log_core.logger -> Raft_udp_pb.configuration -> validations Lwt_stream.t 

end 
