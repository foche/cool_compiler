(* globalvalidator.mli *)

module Abssyn = Parser.Abstractsyntax
module Tbls = Util.Tables
module Tree = Util.Tree

type validator_args = {
  program : Abssyn.program;
  handle_to_class : (Tbls.typ_sym, Abssyn.class_node) Hashtbl.t;
  parents : (Tbls.typ_sym, Tbls.typ_sym) Hashtbl.t;
  sigs : Methodtbl.t;
}

val validate : args:validator_args -> bool * Tbls.typ_sym Tree.t option
