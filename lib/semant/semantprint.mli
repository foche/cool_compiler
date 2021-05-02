(* semantprint.mli *)

module Abssyn = Parser.Abstractsyntax

val print_error : loc:Abssyn.loc -> ('a, Format.formatter, unit) format -> 'a

val print_typecheck_error : unit -> unit
(** [print_typecheck_error ()] prints a typechecking error message to [stderr]. *)
