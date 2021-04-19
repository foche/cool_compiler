(* typestest.ml *)

open Semant
open Util

let cl_typ1 = Tables.make_type "A"

let cl_typ2 = Tables.make_type "B"

let%test "translate_type" =
  Types.translate_type ~cl_typ:cl_typ1 Tables.self_type = cl_typ1
  && Types.translate_type ~cl_typ:cl_typ1 cl_typ2 = cl_typ2

let%test "is_subtype" = true
