
(* 

The representation of lambda-terms.

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

let kind_ = Kind NoInfo
let type_ = Type NoInfo
let var_ x = Var (x, NoInfo)
let lambda_ (x, t) e = Lambda (x, t, e, NoInfo)
let prod_ (x, t) e = Prod (x, t, e, NoInfo)
let app_ t1 t2 = App (t1, t2, NoInfo)
let ref_ n args = Ref (n, args, NoInfo)
                              
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
         
(* Variables and occurrences *)

let rec vars = function
  | Kind _ -> StringSet.empty
  | Type _ -> StringSet.empty
  | Var (x, _) -> StringSet.singleton x
  | Lambda (x, t, e, _) -> StringSet.union (vars t) (vars e)
  | Prod (x, t, e, _) -> StringSet.union (vars t) (vars e)
  | App (t1, t2, _) -> StringSet.union (vars t1) (vars t2)
  | Ref (name, args, _) -> List.fold_left (fun vs t -> StringSet.union vs (vars t)) StringSet.empty args
  
let rec free_vars = function
  | Kind _ -> StringSet.empty
  | Type _ -> StringSet.empty
  | Var (x, _) -> StringSet.singleton x
  | Lambda (x, t, e, _) -> StringSet.union (free_vars t) (StringSet.diff (free_vars e)
                                                                         (StringSet.singleton x))
  | Prod (x, t, e, _) -> StringSet.union (free_vars t) (StringSet.diff (free_vars e)
                                                                       (StringSet.singleton x))
  | App (t1, t2, _) -> StringSet.union (free_vars t1) (free_vars t2)
  | Ref (name, args, _) -> List.fold_left (fun vs t -> StringSet.union vs (free_vars t)) StringSet.empty args
  
let bound_vars t = StringSet.diff (vars t) (free_vars t)
  
let rec binding_vars = function
  | Kind _ -> StringSet.empty
  | Type _ -> StringSet.empty
  | Var (x, _) -> StringSet.empty
  | Lambda (x, t, e, _) -> StringSet.union (StringSet.singleton x)
                                           (StringSet.union (binding_vars t) (binding_vars e))
  | Prod (x, t, e, _) -> StringSet.union (StringSet.singleton x)
                                         (StringSet.union (binding_vars t) (binding_vars e))
  | App (t1, t2, _) -> StringSet.union (binding_vars t1) (binding_vars t2)
  | Ref (name, args, _) -> List.fold_left (fun vs t -> StringSet.union vs (binding_vars t)) StringSet.empty args
  
let rec all_vars = function
  | Kind _ -> StringSet.empty
  | Type _ -> StringSet.empty
  | Var (x, _) -> StringSet.singleton x
  | Lambda (x, t, e, _) -> StringSet.union (StringSet.union (vars t) (vars e))
                                           (StringSet.singleton x)
  | Prod (x, t, e, _) -> StringSet.union (StringSet.union (vars t) (vars e))
                                         (StringSet.singleton x)  
  | App (t1, t2, _) -> StringSet.union (vars t1) (vars t2)
  | Ref (name, args, _) -> List.fold_left (fun vs t -> StringSet.union vs (vars t)) StringSet.empty args
  
(* barendregt convention *)
                         
module Subst = Map.Make(String)

let rec fresh (base:string) (level:int) (forbidden:StringSet.t) : string =
  let candidate = match level with
    | 0 -> base
    | 1 -> base ^ "'"
    | 2 -> base ^ "''"
    | k -> Printf.sprintf "%s-%d" base level
  in if StringSet.mem candidate forbidden
     then fresh base (level + 1) forbidden
     else candidate

let rename x sub =
  try Subst.find x sub
  with Not_found -> x

let rec noclash t forbidden sub =
  let noclash_binder x t e forbidden sub= 
    let x' = fresh x 0 forbidden in
    let forbidden' = StringSet.add x' forbidden in
    let (t', forbidden'') = noclash t forbidden' sub in
    let sub' = Subst.add x x' sub in
    let (e', forbidden''') = noclash e forbidden'' sub'
    in ((x', t', e'), forbidden''')
  and noclash_comp t1 t2 forbidden sub =
    let (t1', forbidden') = noclash t1 forbidden sub in
    let (t2', forbidden'') = noclash t2 forbidden' sub
    in ((t1', t2'), forbidden'')
  in match t with
     | Var (x, info) -> (Var (rename x sub, info), forbidden)
     | Lambda (x, t, e, info) -> let ((x', t', e'), forbidden') = noclash_binder x t e forbidden sub
                                 in (Lambda (x', t', e', info), forbidden')
     | Prod (x, t, e, info) -> let ((x', t', e'), forbidden') = noclash_binder x t e forbidden sub
                               in (Prod (x', t', e', info), forbidden')
     | App (t1, t2, info) -> let ((t1', t2'), forbidden') = noclash_comp t1 t2 forbidden sub
                             in (App (t1', t2', info), forbidden')
     | Ref (name, args, info) -> let (rargs', forbidden''') =
                                   List.fold_left (fun (args', forbidden') arg ->
                                       let (arg', forbidden'') = noclash arg forbidden' sub
                                       in (arg'::args', forbidden'')) ([], forbidden) args 
                                 in (Ref (name, List.rev rargs', info), forbidden''')
     | _ -> (t, forbidden)                               

let noclash_ t =
  let (t', _) = noclash t (StringSet.empty) (Subst.empty)
  in t'

(* substitution *)
   
let var_subst x info sub =
  try Subst.find x sub
  with Not_found -> Var (x, info)
                        
let rec subst_ t forbidden ren sub = match t with
  | Var (x, info) -> (var_subst (rename x ren) info sub
                     , forbidden)
  | Lambda (x, t, e, info) -> let ((x', t', e'), forbidden') = subst_binder x t e forbidden ren sub
                              in (Lambda (x', t', e', info), forbidden')
  | Prod (x, t, e, info) -> let ((x', t', e'), forbidden') = subst_binder x t e forbidden ren sub
                            in (Prod (x', t', e', info), forbidden')
  | App (t1, t2, info) -> let ((t1', t2'), forbidden') = subst_compose t1 t2 forbidden ren sub
                          in (App (t1', t2', info), forbidden')
  | Ref (name, args, info) -> let (rargs', forbidden') = List.fold_left (fun (args, forbidden) arg ->
                                                             let (arg', forbidden') = subst_ arg forbidden ren sub
                                                             in (arg'::args, forbidden')) ([], forbidden) args
                              in (Ref (name, List.rev rargs', info), forbidden')
  | _ -> (t, forbidden)
                          
                               
and subst_binder x t e forbidden ren sub =
  let x' = fresh x 0 forbidden in
  let forbidden' = StringSet.add x' forbidden in
  let (t', forbidden'') = subst_ t forbidden' ren sub in
  let ren' = Subst.add x x' ren in
  let (e', forbidden''') = subst_ e forbidden'' ren' sub
  in ((x', t', e'), forbidden''')

and subst_compose t1 t2 forbidden ren sub =
  let (t1', forbidden') = subst_ t1 forbidden ren sub in
  let (t2', forbidden'') = subst_ t2 forbidden' ren sub in
  ((t1', t2'), forbidden'')

let subst t sub =
  let forbidden = Subst.fold
                    (fun x t forbidden -> StringSet.union forbidden (all_vars t))
                    sub StringSet.empty in
  let (t',_) = subst_ t forbidden Subst.empty sub
  in t'

let subst_one t x u =
  subst t (Subst.singleton x u)

    
(* alpha normalization *)

(*
let alpha_norm t sub level = match t with
  | Var (x, info) -> subst
 *)
