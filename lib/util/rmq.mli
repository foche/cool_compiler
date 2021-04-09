(* rmq.mli *)

type t

val create : int array -> t

val find : t -> int -> int -> int * int
