(* validator.mli *)

module Tbls = Util.Tables

val bind :
  ?accept:unit lazy_t ->
  checker:bool lazy_t ->
  err_fun:unit lazy_t ->
  bool ->
  bool

val is_not_self_var : id:Tbls.id_sym -> bool lazy_t

val is_not_self_type : typ:Tbls.typ_sym -> bool lazy_t
