(* methodtbl.mli *)

module Abssyn = Parser.Abstractsyntax
module Tbls = Util.Tables
module Tree = Util.Tree

type method_sig = {
  method_ret_typ : Tbls.typ_sym;
  formals : Abssyn.var_decl list;
  impl_class : Tbls.typ_sym;
  label : Tbls.id_sym;
}

type t

val create : int -> t

val add :
  t ->
  cl_typ:Tbls.typ_sym ->
  method_id:Tbls.id_sym ->
  method_ret_typ:Tbls.typ_sym ->
  formals:Abssyn.var_decl list ->
  bool

val find_opt :
  t ->
  inherit_tree:Tbls.typ_sym Tree.t ->
  cl_typ:Tbls.typ_sym ->
  method_id:Tbls.id_sym ->
  method_sig option
