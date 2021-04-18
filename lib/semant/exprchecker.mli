(* exprchecker.mli *)

open Parser
open Util

type context = {
  id_env : (Tables.id_sym, Tables.type_sym) Symtbl.t;
  sigs : Methodtbl.t;
  inherit_tree : Tables.type_sym Tree.t;
  cl_typ : Tables.type_sym;
}

val typecheck :
  ctx:context -> expr:Abstractsyntax.expr_node -> Abstractsyntax.expr_node
