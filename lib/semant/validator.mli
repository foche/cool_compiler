(* validator.mli *)

module Tbls = Util.Tables

val fold :
  ?accept:Unit.t Lazy.t ->
  err_fun:Unit.t Lazy.t ->
  fail:'a ->
  success:'a Lazy.t ->
  Bool.t ->
  'a

val map :
  ?accept:Unit.t Lazy.t ->
  pred:Bool.t ->
  err_fun:Unit.t Lazy.t ->
  Bool.t ->
  Bool.t

val is_not_self_var : Tbls.id_sym -> Bool.t

val is_not_self_type : Tbls.typ_sym -> Bool.t
