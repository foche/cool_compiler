(* exprchecker.mli *)

open Parser
open Util

type context = {
    id_env : (Tables.id_sym, Tables.type_sym) Symtbl.t;
    sigs : Methodtbl.t;
    graph : Tables.type_sym Tree.t;
    cl : Ast.clazz;
    filename : Tables.str_sym;
  }

val typecheck :
  ctx : context ->
  exp_node : Ast.expr_node ->
  Ast.expr_node
