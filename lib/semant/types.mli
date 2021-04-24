(* types.mli *)

open Util
module T = Tables

val translate_type : cl_typ:T.typ_sym -> T.typ_sym -> T.typ_sym

val is_subtype :
  T.typ_sym Tree.t ->
  cl_typ:T.typ_sym ->
  sub_typ:T.typ_sym ->
  super_typ:T.typ_sym ->
  bool
