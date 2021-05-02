(* sparsetbl.mli *)

type t

val create : data:Int.t Array.t -> t

val range_min : t -> left:Int.t -> right:Int.t -> Int.t * Int.t
