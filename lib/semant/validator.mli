(* validator.mli *)

open Util

val bind :
  ?accept:unit lazy_t ->
  checker:bool lazy_t ->
  err_fun:unit lazy_t ->
  bool ->
  bool

val is_not_self_var : id:Tables.id_sym -> bool lazy_t

val is_not_self_type : typ:Tables.typ_sym -> bool lazy_t
