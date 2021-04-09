(* typeenv.ml *)

type ('a, 'b) t = 'a list Stack.t * ('a, 'b) Hashtbl.t

let create _ =
  Stack.create (), Hashtbl.create 32

let enter_scope (stack, tbl) lazy_f =
  Stack.push [] stack;
  let result = Lazy.force lazy_f in
  List.iter (Hashtbl.remove tbl) (Stack.pop stack);
  result

let add (stack, tbl) k v =
  let scope = Stack.pop stack in
  let is_duplicate = List.exists ((=) k) scope in
  let is_shadowed = not is_duplicate && Hashtbl.mem tbl k in
  Stack.push (k :: scope) stack;
  Hashtbl.add tbl k v;
  is_duplicate, is_shadowed

let find (_, tbl) =
  Hashtbl.find tbl

let find_opt (_, tbl) =
  Hashtbl.find_opt tbl
