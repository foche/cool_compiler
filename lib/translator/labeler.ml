(* labeler.ml *)

open Util

type label = Tables.id_sym

let label_i = ref 0

let fresh_label _ =
  let i = !label_i in
  let lab = Tables.make_id ("label" ^ string_of_int i) in
  label_i := i + 1;
  lab
