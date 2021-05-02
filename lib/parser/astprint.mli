(* astprint.mli *)

open! StdLabels

val print_ast : Abstractsyntax.program -> Unit.t
(** [print_ast ast] pretty-prints the [ast]. *)

val print_syntax_error : Unit.t -> Unit.t
(** [print_syntax_error ()] prints a syntax error message to stderr. *)

val print_eof_error : String.t -> Unit.t
(**
  [print_eof_error filename] prints a syntax error for an empty program
  with the given [filename].
 *)
