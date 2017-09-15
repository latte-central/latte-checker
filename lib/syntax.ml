
(* 

The representation of lambda-terms 

The 'a type argument 

*)

open Utils

type astinfo =
  | NoInfo
       
type term =
  | Kind of astinfo
  | Type of astinfo
  | Var of string * astinfo
  | Lambda of string * term * term * astinfo
  | Prod of string * term * term * astinfo
  | App of term * term * astinfo
  | Ref of string * (term list) * astinfo
  | Assert of term * term * astinfo

let kind_ = Kind NoInfo
let type_ = Type NoInfo
let var_ x = Var (x, NoInfo)
let lambda_ (x, t) e = Lambda (x, t, e, NoInfo)
let prod_ (x, t) e = Prod (x, t, e, NoInfo)
let app_ t1 t2 = App (t1, t2, NoInfo)
let ref_ n args = Ref (n, args, NoInfo)
let ascribe_ e t = Assert (e, t, NoInfo)
                              
let rec string_of_term = function
  | Kind _ -> ":kind"
  | Type _ -> ":type"
  | Var (x, _) -> x
  | Lambda (x, t, e, _)

    -> Printf.sprintf "(lambda [%s %s] %s)"
         x (string_of_term t) (string_of_term e)
  | Prod (x, t, e, _)
    -> Printf.sprintf "(prod [%s %s] %s)"
         x (string_of_term t) (string_of_term e)
  | App (t1, t2, _)
    -> Printf.sprintf "[%s %s]" (string_of_term t1) (string_of_term t2)
  | Ref (name, args, _)
    -> Printf.sprintf "(%s %s)" name (string_of_list string_of_term "" "" " " args)
  | Assert (e, t, _)
    -> Printf.sprintf "(::ascribe %s %s)" (string_of_term e) (string_of_term t)

         
                                    
                                    
                                    
                                    
