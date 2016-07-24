(** Common testing utility *)



type test_env 

val make_env : nb_of_keys:int -> unit -> test_env 

type test 

val make_test : env:test_env -> nb_of_transfers:int -> unit -> test 

val is_done : test -> bool 

module type App_sig = sig 

  type t 

  val content_of_url : string -> string Lwt.t 

  val handle_tx : t -> Asset_pb.tx -> t Lwt.t  

end 

module Make(App:App_sig) : sig 

  val execute_test : test -> App.t -> (test * App.t) Lwt.t  

end 

