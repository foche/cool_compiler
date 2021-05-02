(* typecheck.mli *)

module Abssyn = Parser.Abstractsyntax
module Layout = Translator.Objectlayout

val semant_verbose : Bool.t ref
(** [semant_verbose] is a flag that enables AST printing after typechecking. *)

val typecheck : (module Layout.S) -> Abssyn.program -> Abssyn.program Option.t
