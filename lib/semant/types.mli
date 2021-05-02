(* types.mli *)

module Tbls = Util.Tables
module Tree = Util.Tree

val translate_type : cl_typ:Tbls.typ_sym -> Tbls.typ_sym -> Tbls.typ_sym

val is_subtype :
  Tbls.typ_sym Tree.t ->
  cl_typ:Tbls.typ_sym ->
  sub_typ:Tbls.typ_sym ->
  super_typ:Tbls.typ_sym ->
  Bool.t
