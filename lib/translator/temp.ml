(* temp.ml *)

open Util

type t = int

type label = Tables.id_sym

let temp_i = ref 0

let label_i = ref 0

let create_temp _ =
  let i = !temp_i in
  temp_i := i + 1;
  i

let create_label _ =
  let i = !label_i in
  let lab = Tables.make_id ("label" ^ string_of_int i) in
  label_i := i + 1;
  lab
