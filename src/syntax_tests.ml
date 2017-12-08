
open OUnit2

open Utils
open Syntax

(* basic ast tests*)
       
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

(* variables tests *)
let vars_test1 ctx = assert_equal (string_of_stringset (vars (lambda_ ("x", var_ "T") (var_ "x"))))
                                  "{T,x}"
let vars_test2 ctx = assert_equal (string_of_stringset (vars (lambda_ ("x", var_ "T") (app_ (var_ "x")
                                                                                            (var_ "y")))))
                                  "{T,x,y}"
let vars_test3 ctx = assert_equal (string_of_stringset (free_vars (lambda_ ("x", var_ "T") (app_ (var_ "x")
                                                                                                 (var_ "y")))))
                                  "{T,y}"
let vars_test4 ctx = assert_equal (string_of_stringset (bound_vars (lambda_ ("x", var_ "T") (app_ (var_ "x")
                                                                                                  (var_ "y")))))
                                  "{x}"
let vars_test5 ctx = assert_equal (string_of_stringset (binding_vars (lambda_ ("x", var_ "T") (app_ (var_ "x")
                                                                                                    (var_ "y")))))
                                  "{x}"
let vars_test6 ctx = assert_equal (string_of_stringset (bound_vars (lambda_ ("z", var_ "U") (lambda_ ("x", var_ "T") (app_ (var_ "x")
                                                                                                                           (var_ "y"))))))
                                  "{x}"
let vars_test7 ctx = assert_equal (string_of_stringset (binding_vars (lambda_ ("z", var_ "U") (lambda_ ("x", var_ "T") (app_ (var_ "x")
                                                                                                                             (var_ "y"))))))
                                  "{x,z}"

let vars_test8 ctx = assert_equal (string_of_stringset (all_vars (lambda_ ("z", var_ "U") (lambda_ ("x", var_ "T") (app_ (var_ "x")
                                                                                                                         (var_ "y"))))))
                                  "{T,U,x,y,z}"

let vars_suite =
  "vars">:::
    ["test1" >:: vars_test1
    ; "test2" >:: vars_test2
    ; "test3" >:: vars_test3
    ; "test4" >:: vars_test4
    ; "test5" >:: vars_test5
    ; "test6" >:: vars_test6
    ; "test7" >:: vars_test7
    ; "test8" >:: vars_test8
    ] ;;

(* barendregt convention *)
let noclash_test1 ctx = assert_equal (string_of_term (noclash_ (lambda_ ("x", var_ "T") (var_ "x"))))
                                   "(lambda [x T] x)"
let noclash_test2 ctx = assert_equal (string_of_term (noclash_ (lambda_ ("x", var_ "T")
                                                                         (lambda_ ("x", var_ "U") (var_ "x")))))
                                   "(lambda [x T] (lambda [x' U] x'))"
let noclash_test3 ctx = assert_equal (string_of_term (noclash_ (lambda_ ("x", var_ "T")
                                                                         (lambda_ ("x", var_ "U") (ref_ "test" [(var_"x"); (var_ "y")])))))
                                   "(lambda [x T] (lambda [x' U] (test x' y)))"
                                   
let noclash_suite =
  "noclash">:::
    ["test1" >:: noclash_test1
    ; "test2" >:: noclash_test2
    ; "test3" >:: noclash_test3
    ] ;;

(* substitution *)
let subst_test1 ctx = assert_equal (string_of_term (subst_one (var_ "x") "x" type_))
                        ":type"
let subst_test2 ctx = assert_equal (string_of_term (subst_one (var_ "y") "x" type_))
                        "y"
let subst_test3 ctx = assert_equal (string_of_term (subst_one (app_ (var_ "y") (var_ "x")) "x" type_))
                        "[y :type]"

let subst_test4 ctx = assert_equal (string_of_term (subst_one (prod_ ("_", prod_ ("x'", var_ "T")
                                                                             (prod_ ("_", prod_ ("x", var_ "T")
                                                                                            (prod_ ("_", app_ (var_ "X") (var_ "x"))
                                                                                               (app_ (app_ (var_ "R") (var_ "x"))
                                                                                                  (var_ "x'"))))
                                                                                (app_ (var_ "R") (var_ "z"))))
                                                                 (app_ (var_ "R") (var_ "z"))) "z" (var_ "x")))
                        "(prod [_ (prod [x' T] (prod [_' (prod [x'' T] (prod [_'' [X x'']] [[R x''] x']))] [R x]))] [R x])"
                        

let subst_suite =
  "subst">:::
    ["test1" >:: subst_test1
    ; "test2" >:: subst_test2
    ; "test3" >:: subst_test3
    ; "test4" >:: subst_test4
    ] ;;

let _ =
  Printf.printf "===== Running tests =====\n" ;
  Printf.printf "==> suite 'ast'\n" ;
  run_test_tt_main ast_suite ;
  Printf.printf "==> suite 'vars'\n" ;
  run_test_tt_main vars_suite ;
  Printf.printf "==> suite 'noclash'\n" ;
  run_test_tt_main subst_suite ;
  Printf.printf "==> suite 'subst'\n" ;
  (* Printf.printf "%s" (string_of_term (subst_one (prod_ ("_", prod_ ("x'", var_ "T")
   *                                                              (prod_ ("_", prod_ ("x", var_ "T")
   *                                                                             (prod_ ("_", app_ (var_ "X") (var_ "x"))
   *                                                                                (app_ (app_ (var_ "R") (var_ "x"))
   *                                                                                   (var_ "x'"))))
   *                                                                 (app_ (var_ "R") (var_ "z"))))
   *                                                      (app_ (var_ "R") (var_ "z"))) "z" (var_ "x"))) ; *)
  run_test_tt_main subst_suite



