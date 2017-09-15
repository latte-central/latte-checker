
open OUnit2

open Syntax
     
let ast_test1 ctx = assert_equal (string_of_term (Kind NoInfo)) ":kind"
                                 
let ast_suite =
  "ast">:::
    ["test1" >:: ast_test1] ;;

let _ =
  Printf.printf "===== Running tests =====\n" ;
  Printf.printf "==> suite 'ast'\n" ;
  run_test_tt_main ast_suite

    
