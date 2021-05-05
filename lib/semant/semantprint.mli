(* semantprint.mli *)

module Loc = Parser.Location

val print_error : loc:Loc.t -> ('a, Format.formatter, Unit.t) format -> 'a

val print_typecheck_error : Unit.t -> Unit.t
(** [print_typecheck_error ()] prints a typechecking error message to [stderr]. *)
