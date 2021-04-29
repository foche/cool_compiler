(* typecheck.mli *)

open Parser
module Layout = Translator.Objectlayout

val semant_verbose : bool ref
(** [semant_verbose] is a flag that enables AST printing after typechecking. *)

val typecheck :
  (module Layout.S) -> Abstractsyntax.program -> Abstractsyntax.program option
