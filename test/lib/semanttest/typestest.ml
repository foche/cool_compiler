(* typestest.ml *)

open Semant
open Util
open Testutil
module Tutil = Treeutil
module T = Tables

let cl_typ1 = T.make_type "A"

let cl_typ2 = T.make_type "B"

let%test "translate_type" =
  Types.translate_type ~cl_typ:cl_typ1 T.self_type = cl_typ1
  && Types.translate_type ~cl_typ:cl_typ1 cl_typ2 = cl_typ2

let%test "is_subtype" =
  Types.is_subtype Tutil.tree ~cl_typ:T.main_type ~sub_typ:Tutil.cl_g
    ~super_typ:Tutil.cl_c
  && Types.is_subtype Tutil.tree ~cl_typ:T.main_type ~sub_typ:Tutil.cl_c
       ~super_typ:Tutil.cl_c
  && Types.is_subtype Tutil.tree ~cl_typ:T.main_type ~sub_typ:Tutil.cl_c
       ~super_typ:Tutil.cl_g
     |> not
  && Types.is_subtype Tutil.tree ~cl_typ:T.main_type ~sub_typ:Tutil.cl_c
       ~super_typ:T.main_type
     |> not
  && Types.is_subtype Tutil.tree ~cl_typ:T.main_type ~sub_typ:T.self_type
       ~super_typ:T.self_type
  && Types.is_subtype Tutil.tree ~cl_typ:T.main_type ~sub_typ:T.main_type
       ~super_typ:T.self_type
     |> not
  && Types.is_subtype Tutil.tree ~cl_typ:T.main_type ~sub_typ:T.self_type
       ~super_typ:T.main_type
  && Types.is_subtype Tutil.tree ~cl_typ:T.main_type ~sub_typ:T.self_type
       ~super_typ:T.io_type
