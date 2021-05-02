(* types.ml *)

module Tbls = Util.Tables
module Tree = Util.Tree

let translate_type ~cl_typ typ = if typ = Tbls.self_type then cl_typ else typ

let is_subtype inherit_tree ~cl_typ ~sub_typ ~super_typ =
  sub_typ = super_typ
  || super_typ <> Tbls.self_type
     && translate_type ~cl_typ sub_typ
        |> Tree.is_ancestor inherit_tree ~ancestor:super_typ
