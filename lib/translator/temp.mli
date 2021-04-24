(* temp.mli *)

type t

type temp

val create : unit -> t

val fresh_temp : t -> temp

val print : Format.formatter -> temp -> unit
