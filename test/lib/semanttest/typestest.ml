(* typestest.ml *)

open Semant
open Util
open Testutil
module Tutil = Treeutil

let cl_typ1 = Tables.make_type "A"

let cl_typ2 = Tables.make_type "B"

let%test "translate_type" =
  Types.translate_type ~cl_typ:cl_typ1 Tables.self_type = cl_typ1
  && Types.translate_type ~cl_typ:cl_typ1 cl_typ2 = cl_typ2

let%test "is_subtype" =
  Types.is_subtype Tutil.tree ~cl_typ:Tables.main_type ~sub_typ:Tutil.cl_g
    ~super_typ:Tutil.cl_c
  && Types.is_subtype Tutil.tree ~cl_typ:Tables.main_type ~sub_typ:Tutil.cl_c
       ~super_typ:Tutil.cl_c
  && Types.is_subtype Tutil.tree ~cl_typ:Tables.main_type ~sub_typ:Tutil.cl_c
       ~super_typ:Tutil.cl_g
     |> not
  && Types.is_subtype Tutil.tree ~cl_typ:Tables.main_type ~sub_typ:Tutil.cl_c
       ~super_typ:Tables.main_type
     |> not
  && Types.is_subtype Tutil.tree ~cl_typ:Tables.main_type
       ~sub_typ:Tables.self_type ~super_typ:Tables.self_type
  && Types.is_subtype Tutil.tree ~cl_typ:Tables.main_type
       ~sub_typ:Tables.main_type ~super_typ:Tables.self_type
     |> not
  && Types.is_subtype Tutil.tree ~cl_typ:Tables.main_type
       ~sub_typ:Tables.self_type ~super_typ:Tables.main_type
  && Types.is_subtype Tutil.tree ~cl_typ:Tables.main_type
       ~sub_typ:Tables.self_type ~super_typ:Tables.io_type
