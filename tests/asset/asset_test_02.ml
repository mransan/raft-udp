module Pb = Asset_pb
module Cry = Raft_cry
module Test_utl = Asset_test_utl

open Lwt.Infix

module Test_utl_exec = Test_utl.Make(Asset_app) 

let main () = 
  (* Configuration *)

  let nb_of_tests = 200 in 
  let max_nb_of_transfers = 200 in 
  let nb_of_keys = 200 in 

  (* Create the tests *)

  let env = Test_utl.make_env ~nb_of_keys () in 
  let tests = 
    let rec aux = function
      | 0 -> [] 
      | n -> 
        let nb_of_transfers = Random.int max_nb_of_transfers + 1 in 
        (Test_utl.make_test ~env ~nb_of_transfers ()) :: (aux @@ n - 1)
    in 
    aux nb_of_tests
  in 

  let execute_nth_test ~nth ~app l = 
    let rec aux nth l = 
      match nth, l with
      | 0, test::tl -> 
        if Test_utl.is_done test 
        then 
          Lwt.return (tl, app) 
        else 
          Test_utl_exec.execute_test test app >|=(fun (test, app) -> (test::tl, app))

      | nth, test::tl -> 
        aux (nth - 1) tl  
        >|=(fun (tests, app) -> (test::tests, app))

      | _ -> 
        Printf.eprintf "Programatic error\n";
        assert(false)
    in 
    aux nth l 
  in 

  let rec execute_all_tests app = function
    | [] -> 
      Lwt_io.printf 
        "- App after all tests:\n> %s\n" 
        (Asset_app.show app); 
    | l  -> 
      let nth = Random.int @@ List.length l in  
      execute_nth_test ~nth ~app l
      >>=(fun (l, app) ->
        execute_all_tests app l 
      )
  in 
  
  execute_all_tests (Asset_app.make ()) tests  

let () = Random.self_init () 
let () = Lwt_main.run (main ()) 
