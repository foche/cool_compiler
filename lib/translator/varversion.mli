(* varversion.mli *)

open Util

type t

val create : int -> t

val find : t -> Tables.id_sym -> int

val add : t -> Tables.id_sym -> int
