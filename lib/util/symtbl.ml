(* symtbl.ml *)

open StdLabels
open MoreLabels

type ('a, 'b) t = 'a list Stack.t * ('a, 'b) Hashtbl.t

let create n =
  let stack = Stack.create () in
  Stack.push [] stack ;
  (stack, Hashtbl.create n)

let enter_scope (stack, tbl) ~cont =
  Stack.push [] stack ;
  let result = Lazy.force cont in
  List.iter ~f:(Hashtbl.remove tbl) (Stack.pop stack) ;
  result

let add (stack, tbl) ~k ~v =
  let scope = Stack.pop stack in
  let is_duplicate = List.exists ~f:(( = ) k) scope in
  let is_shadowed = (not is_duplicate) && Hashtbl.mem tbl k in
  Stack.push (k :: scope) stack ;
  Hashtbl.add tbl ~key:k ~data:v ;
  (is_duplicate, is_shadowed)

let find (_, tbl) ~k = Hashtbl.find tbl k

let find_opt (_, tbl) ~k = Hashtbl.find_opt tbl k
