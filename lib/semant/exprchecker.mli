(* exprchecker.mli *)

module Abssyn = Parser.Abstractsyntax
module Symtbl = Util.Symtbl
module Tbls = Util.Tables
module Tree = Util.Tree

type context = {
  id_env : (Tbls.id_sym, Tbls.typ_sym) Symtbl.t;
  sigs : Methodtbl.t;
  inherit_tree : Tbls.typ_sym Tree.t;
  cl_typ : Tbls.typ_sym;
}

val typecheck :
  ?super_typ:Tbls.typ_sym ->
  is_tail_pos:Bool.t ->
  ctx:context ->
  Abssyn.expr_node ->
  Abssyn.expr_node
