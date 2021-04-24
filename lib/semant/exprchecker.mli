(* exprchecker.mli *)

open Parser
open Util

type context = {
  id_env : (Tables.id_sym, Tables.typ_sym) Symtbl.t;
  sigs : Methodtbl.t;
  inherit_tree : Tables.typ_sym Tree.t;
  cl_typ : Tables.typ_sym;
}

val typecheck :
  ?super_typ:Tables.typ_sym ->
  ctx:context ->
  Abstractsyntax.expr_node ->
  Abstractsyntax.expr_node
