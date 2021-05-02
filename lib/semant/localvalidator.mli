(* localvalidator.mli *)

module Abssyn = Parser.Abstractsyntax
module Symtbl = Util.Symtbl
module Tbls = Util.Tables
module Tree = Util.Tree

type validator_args = {
  id_env : (Tbls.id_sym, Tbls.typ_sym) Symtbl.t;
  func_env : (Tbls.id_sym, Abssyn.method_def) Symtbl.t;
  inherit_tree : Tbls.typ_sym Tree.t;
  sigs : Methodtbl.t;
  untyped_classes : (Tbls.typ_sym, Abssyn.class_node) Hashtbl.t;
  typed_classes : (Tbls.typ_sym, Abssyn.class_node) Hashtbl.t;
}

val validate : args:validator_args -> bool
