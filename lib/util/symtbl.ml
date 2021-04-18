(* symtbl.ml *)

open StdLabels
open MoreLabels

type ('a, 'b) t = 'a list Stack.t * ('a, 'b) Hashtbl.t

let create n =
  let stack = Stack.create () in
  Stack.push [] stack;
  (stack, Hashtbl.create n)

let enter_scope (stack, tbl) ~cont =
  Stack.push [] stack;
  let result = Lazy.force cont in
  List.iter ~f:(Hashtbl.remove tbl) (Stack.pop stack);
  result

let add (stack, tbl) ~key ~data =
  let scope = Stack.pop stack in
  let is_duplicate = List.exists ~f:(( = ) key) scope in
  let is_shadowed = (not is_duplicate) && Hashtbl.mem tbl key in
  Stack.push (key :: scope) stack;
  Hashtbl.add tbl ~key ~data;
  (is_duplicate, is_shadowed)

let find (_, tbl) = Hashtbl.find tbl

let find_opt (_, tbl) = Hashtbl.find_opt tbl
