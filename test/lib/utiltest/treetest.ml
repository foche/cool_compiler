(* treetest.ml *)

open StdLabels
open MoreLabels
open Util
open Testutil
module Tutil = Treeutil
module Hutil = Hashtblutil

let cl_h = Tables.make_type "H"

let cl_i = Tables.make_type "I"

let cl_j = Tables.make_type "J"

let cl_k = Tables.make_type "K"

let cl_l = Tables.make_type "L"

let cl_foo = Tables.make_type "Foo"

let%test "Cycle" =
  let parents = Tutil.create_tree_parents () in
  let cycle =
    Hutil.init 17 [ (cl_h, cl_i); (cl_i, cl_j); (cl_j, cl_k); (cl_k, cl_h) ]
  in
  Hutil.add_all parents cycle;
  match Tree.create ~parents ~root:Tutil.root with
  | Cycle vert -> Hashtbl.mem cycle vert
  | _ -> false

let%test "Disconnected" =
  let parents = Tutil.create_tree_parents () in
  let disconnected =
    Hutil.init 17 [ (cl_h, cl_i); (cl_i, cl_j); (cl_j, cl_k); (cl_k, cl_l) ]
  in
  Hutil.add_all parents disconnected;
  match Tree.create ~parents ~root:Tutil.root with
  | Disconnected (vert, parent) -> vert = cl_k && parent = cl_l
  | _ -> false

let%test "is_leaf" =
  Tree.is_leaf Tutil.tree Tables.int_type
  && Tree.is_leaf Tutil.tree Tutil.cl_d
  && Tree.is_leaf Tutil.tree Tutil.root |> not
  && Tree.is_leaf Tutil.tree Tutil.cl_f |> not

let%test "mem" =
  Tree.mem Tutil.tree Tables.int_type
  && Tree.mem Tutil.tree Tutil.cl_g
  && Tree.mem Tutil.tree Tutil.cl_a
  && Tree.mem Tutil.tree Tutil.root
  && Tree.mem Tutil.tree cl_foo |> not

let%test "find_parent_opt" =
  Tree.find_parent_opt Tutil.tree Tables.int_type = Some Tutil.root
  && Tree.find_parent_opt Tutil.tree Tutil.cl_g = Some Tutil.cl_f
  && Tree.find_parent_opt Tutil.tree Tutil.cl_b = Some Tutil.cl_a
  && Tree.find_parent_opt Tutil.tree Tutil.cl_a = Some Tutil.root
  && Tree.find_parent_opt Tutil.tree Tutil.root = None
  && Tree.find_parent_opt Tutil.tree cl_foo = None

let%test "lca" =
  Tree.lca Tutil.tree ~vert1:Tutil.root ~vert2:Tutil.root = Tutil.root
  && Tree.lca Tutil.tree ~vert1:Tutil.cl_a ~vert2:Tutil.cl_a = Tutil.cl_a
  && Tree.lca Tutil.tree ~vert1:Tables.int_type ~vert2:Tables.int_type
     = Tables.int_type
  && Tree.lca Tutil.tree ~vert1:Tables.int_type ~vert2:Tutil.cl_a = Tutil.root
  && Tree.lca Tutil.tree ~vert1:Tutil.cl_a ~vert2:Tables.int_type = Tutil.root
  && Tree.lca Tutil.tree ~vert1:Tutil.cl_g ~vert2:Tutil.cl_a = Tutil.cl_a
  && Tree.lca Tutil.tree ~vert1:Tutil.cl_a ~vert2:Tutil.cl_g = Tutil.cl_a
  && Tree.lca Tutil.tree ~vert1:Tutil.cl_f ~vert2:Tutil.cl_d = Tutil.cl_c
  && Tree.lca Tutil.tree ~vert1:Tables.main_type ~vert2:Tutil.cl_d = Tutil.root
  && Tree.lca Tutil.tree ~vert1:Tables.main_type ~vert2:Tables.main_type
     = Tables.main_type

let%test "find_out_edges" =
  let root_edges = Tree.find_out_edges Tutil.tree Tutil.root in
  let expected_edges =
    Hutil.set_from_list
      [
        Tables.int_type;
        Tables.io_type;
        Tables.string_type;
        Tables.bool_type;
        Tutil.cl_a;
      ]
  in
  List.for_all ~f:(Hashtbl.mem expected_edges) root_edges
  && Tree.find_out_edges Tutil.tree Tutil.cl_f = [ Tutil.cl_g ]
  && Tree.find_out_edges Tutil.tree Tutil.cl_e = []

let%test "is_ancestor" =
  Tree.is_ancestor Tutil.tree ~ancestor:Tutil.root Tutil.root
  && Tree.is_ancestor Tutil.tree ~ancestor:Tutil.cl_a Tutil.cl_a
  && Tree.is_ancestor Tutil.tree ~ancestor:Tables.int_type Tables.int_type
  && Tree.is_ancestor Tutil.tree ~ancestor:Tables.int_type Tutil.cl_a |> not
  && Tree.is_ancestor Tutil.tree ~ancestor:Tutil.cl_a Tables.int_type |> not
  && Tree.is_ancestor Tutil.tree ~ancestor:Tutil.cl_g Tutil.cl_a |> not
  && Tree.is_ancestor Tutil.tree ~ancestor:Tutil.cl_a Tutil.cl_g
  && Tree.is_ancestor Tutil.tree ~ancestor:Tutil.cl_f Tutil.cl_d |> not
  && Tree.is_ancestor Tutil.tree ~ancestor:Tutil.cl_f Tutil.cl_b |> not
  && Tree.is_ancestor Tutil.tree ~ancestor:Tutil.cl_b Tutil.cl_f
  && Tree.is_ancestor Tutil.tree ~ancestor:Tables.main_type Tutil.cl_d |> not
  && Tree.is_ancestor Tutil.tree ~ancestor:Tables.main_type Tables.main_type
