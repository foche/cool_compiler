(* sparsetbl.mli *)

type t

val create : data:int array -> t

val range_min : t -> left:int -> right:int -> int * int
