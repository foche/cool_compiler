(* optutil.mli *)

val fold2 : none:'a -> some:('b -> 'c -> 'a) -> 'b option -> 'c option -> 'a

val map2 : f:('a -> 'b -> 'c) -> 'a option -> 'b option -> 'c option

val singleton : 'a option -> 'a list option

val merge : 'a option -> 'a list option -> 'a list option

val flatten_opt_list : 'a option list -> 'a list option
