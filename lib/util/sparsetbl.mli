(* sparsetbl.mli *)

type t

val create : int array -> t

val range_min : t -> int -> int -> int * int
