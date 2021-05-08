(* validator.ml *)

module Tbls = Util.Tables

let fold ?accept ~err_fun ~fail ~success pred =
  if pred then (
    Option.iter (fun cont -> Lazy.force cont) accept;
    Lazy.force success)
  else (
    Lazy.force err_fun;
    fail)

let map ?accept ~pred ~err_fun acc =
  fold ?accept ~err_fun ~fail:false ~success:(lazy acc) pred

let is_not_self_var id = id <> Tbls.self_var

let is_not_self_type typ = typ <> Tbls.self_type
