
type validation = 
  | Ok 
  | Error of string 

module type App_sig  = sig 

  type tx 

  val decode : bytes -> tx  

  val validate : tx -> validation 

end

module Make(App:App_sig) : sig 

  val start : Lwt_log_core.logger -> Raft_udp_pb.configuration -> unit Lwt.t  

end 
