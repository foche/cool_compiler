(* exprchecker.mli *)

open Parser
open Util

type context = {
  id_env : (Tables.id_sym, Tables.type_sym) Symtbl.t;
  sigs : Methodtbl.t;
  graph : Tables.type_sym Tree.t;
  cl : Abstractsyntax.class_node;
}

val typecheck :
  ctx:context -> expr:Abstractsyntax.expr_node -> Abstractsyntax.expr_node
