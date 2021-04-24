(* methodtbl.mli *)

open Parser
open Util

type method_sig = {
  ret_typ : Tables.typ_sym;
  formals : Abstractsyntax.var_decl list;
  impl_class : Tables.typ_sym;
  label : Tables.id_sym;
}

type t

val create : int -> t

val add :
  t ->
  typ:Tables.typ_sym ->
  method_id:Tables.id_sym ->
  ret_typ:Tables.typ_sym ->
  formals:Abstractsyntax.var_decl list ->
  bool

val find_opt :
  t ->
  inherit_tree:Tables.typ_sym Tree.t ->
  typ:Tables.typ_sym ->
  method_id:Tables.id_sym ->
  method_sig option
