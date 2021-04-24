(* semantprint.mli *)

open Parser

val print_error :
  loc:Abstractsyntax.loc -> ('a, Format.formatter, unit) format -> 'a

val print_typecheck_error : unit -> unit
(** [print_typecheck_error ()] prints a typechecking error message to [stderr]. *)
