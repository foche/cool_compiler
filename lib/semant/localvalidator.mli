(* localvalidator.mli *)

open Parser
open Util

type validator_args = {
  id_env : (Tables.id_sym, Tables.typ_sym) Symtbl.t;
  func_env : (Tables.id_sym, Abstractsyntax.method_def) Symtbl.t;
  inherit_tree : Tables.typ_sym Tree.t;
  sigs : Methodtbl.t;
  untyped_classes : (Tables.typ_sym, Abstractsyntax.class_node) Hashtbl.t;
  typed_classes : (Tables.typ_sym, Abstractsyntax.class_node) Hashtbl.t;
}

val validate : args:validator_args -> bool
