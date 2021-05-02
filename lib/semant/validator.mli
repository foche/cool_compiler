(* validator.mli *)

module Tbls = Util.Tables

val bind :
  ?accept:Unit.t Lazy.t ->
  checker:Bool.t Lazy.t ->
  err_fun:Unit.t Lazy.t ->
  Bool.t ->
  Bool.t

val is_not_self_var : id:Tbls.id_sym -> Bool.t Lazy.t

val is_not_self_type : typ:Tbls.typ_sym -> Bool.t Lazy.t
