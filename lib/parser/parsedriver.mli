(* parsedriver.mli *)

val parser_verbose : bool ref
(** [parser_verbose] is a flag that enables AST printing after parsing. *)

val parse : string list -> Abstractsyntax.program option
(**
  [parse filenames] parses source files given by [filenames] into an AST.
  If parsing is successful, returns [Some program], where [program] is an
  untyped AST representing the source program. Otherwise, returns [None].
 *)
