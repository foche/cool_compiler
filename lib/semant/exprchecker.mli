(* exprchecker.mli *)

open Parser
open Util

type context = {
    id_env : (Ast.id_sym, Ast.type_sym) Typeenv.t;
    sigs : Methodtbl.t;
    graph : Ast.type_sym Tree.t;
    cl : Ast.clazz;
    filename : Ast.str_sym;
  }

val typecheck :
  ctx : context ->
  exp_node : Ast.expr_node ->
  Ast.expr_node
