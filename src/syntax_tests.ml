
open OUnit2

open Syntax
     
let ast_test1 ctx = assert_equal (string_of_term kind_) ":kind"
let ast_test2 ctx = assert_equal (string_of_term type_) ":type"
let ast_test3 ctx = assert_equal (string_of_term (var_ "x")) "x"
let ast_test4 ctx = assert_equal (string_of_term (lambda_ ("x", var_ "T") (var_ "x")))
                      "(lambda [x T] x)"
let ast_test5 ctx = assert_equal (string_of_term (prod_ ("x", var_ "T") (var_ "x")))
                      "(prod [x T] x)"
let ast_test6 ctx = assert_equal (string_of_term (ref_ "test" [var_ "x"; var_ "y"]))
                      "(test x y)"
                                 
let ast_suite =
  "ast">:::
    ["test1" >:: ast_test1
    ; "test2" >:: ast_test2
    ; "test3" >:: ast_test3
    ; "test4" >:: ast_test4
    ; "test5" >:: ast_test5
    ; "test6" >:: ast_test6
    ] ;;

let _ =
  Printf.printf "===== Running tests =====\n" ;
  Printf.printf "==> suite 'ast'\n" ;
  run_test_tt_main ast_suite

    
