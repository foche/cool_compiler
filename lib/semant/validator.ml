(* validator.ml *)

open Util

let bind ?(accept = lazy ()) ~checker ~err_fun acc =
  if Lazy.force checker then (
    Lazy.force accept;
    acc)
  else (
    Lazy.force err_fun;
    false)

let is_not_self_var ~id = lazy (id <> Tables.self_var)

let is_not_self_type ~typ = lazy (typ <> Tables.self_type)
