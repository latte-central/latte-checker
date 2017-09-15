
let string_of_list (tostr: 'a -> string) (opn:string) (close:string) (delim:string)
      (l:'a list) : string = 
  let rec aux = function 
    | [] -> ""
    | [e] -> tostr e
    | (e::l) -> Printf.sprintf "%s%s%s" (tostr e) delim (aux l)
  in
  Printf.sprintf "%s%s%s" opn (aux l) close

                                 
