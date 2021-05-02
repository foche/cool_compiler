(* parsedriver.mli *)

open! StdLabels

val parser_verbose : Bool.t ref
(** [parser_verbose] is a flag that enables AST printing after parsing. *)

val parse : String.t List.t -> Abstractsyntax.program Option.t
(**
  [parse filenames] parses source files given by [filenames] into an AST.
  If parsing is successful, returns [Some program], where [program] is an
  untyped AST representing the source program. Otherwise, returns [None].
 *)
