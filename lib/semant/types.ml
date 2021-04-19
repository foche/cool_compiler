(* types.ml *)

open Util
module T = Tables

let translate_type ~cl_typ typ = if typ = T.self_type then cl_typ else typ

let is_subtype inherit_tree ~cl_typ ~sub_typ ~super_typ =
  sub_typ = super_typ
  || super_typ <> T.self_type
     && translate_type ~cl_typ sub_typ
        |> Tree.is_ancestor inherit_tree ~ancestor:super_typ
