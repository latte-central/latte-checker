
                                 
(* common modules *)

module StringSet = Set.Make(String)

let list_of_stringset s =
  List.rev (StringSet.fold (fun elem l -> elem::l) s [])
                           
(* various print helpers *)

let string_of_list (tostr: 'a -> string) (opn:string) (close:string) (delim:string)
      (l:'a list) : string = 
  let rec aux = function 
    | [] -> ""
    | [e] -> tostr e
    | (e::l) -> Printf.sprintf "%s%s%s" (tostr e) delim (aux l)
  in
  Printf.sprintf "%s%s%s" opn (aux l) close

                 
let string_of_stringset s = string_of_list (fun x -> x) "{" "}" "," (list_of_stringset s)
