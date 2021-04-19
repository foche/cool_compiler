(* treetest.ml *)

open StdLabels
open MoreLabels
open Util
module Hutil = Hashtblutil

let root = "Object"

let create_tree_parents _ =
  Hutil.init 31
    [
      ("Int", root);
      ("IO", root);
      ("String", root);
      ("Bool", root);
      ("Main", "IO");
      ("A", root);
      ("B", "A");
      ("C", "B");
      ("D", "C");
      ("E", "B");
      ("F", "C");
      ("G", "F");
    ]

let tree =
  let parents = create_tree_parents () in
  match Tree.create ~parents ~root with
  | Tree tree ->
      Hashtbl.add parents ~key:root ~data:root;
      (* should not affect anything *)
      tree
  | _ -> failwith "Could not create tree"

let%test "Cycle" =
  let parents = create_tree_parents () in
  let cycle =
    Hutil.init 17 [ ("H", "I"); ("I", "J"); ("J", "K"); ("K", "H") ]
  in
  Hutil.add_all parents cycle;
  match Tree.create ~parents ~root with
  | Cycle vert -> Hashtbl.mem cycle vert
  | _ -> false

let%test "Disconnected" =
  let parents = create_tree_parents () in
  let disconnected =
    Hutil.init 17 [ ("H", "I"); ("I", "J"); ("J", "K"); ("K", "L") ]
  in
  Hutil.add_all parents disconnected;
  match Tree.create ~parents ~root with
  | Disconnected (vert, parent) -> vert = "K" && parent = "L"
  | _ -> false

let%test "is_leaf" =
  Tree.is_leaf tree "Int" && Tree.is_leaf tree "D"
  && Tree.is_leaf tree root |> not
  && Tree.is_leaf tree "F" |> not

let%test "mem" =
  Tree.mem tree "Int" && Tree.mem tree "G" && Tree.mem tree "A"
  && Tree.mem tree root
  && Tree.mem tree "Foo" |> not
  && Tree.mem tree "Bar" |> not

let%test "find_parent_opt" =
  Tree.find_parent_opt tree "Int" = Some root
  && Tree.find_parent_opt tree "G" = Some "F"
  && Tree.find_parent_opt tree "B" = Some "A"
  && Tree.find_parent_opt tree "A" = Some root
  && Tree.find_parent_opt tree root = None
  && Tree.find_parent_opt tree "Foo" = None
  && Tree.find_parent_opt tree "Bar" = None

let%test "lca" =
  Tree.lca tree ~vert1:root ~vert2:root = root
  && Tree.lca tree ~vert1:"A" ~vert2:"A" = "A"
  && Tree.lca tree ~vert1:"Int" ~vert2:"Int" = "Int"
  && Tree.lca tree ~vert1:"Int" ~vert2:"A" = root
  && Tree.lca tree ~vert1:"A" ~vert2:"Int" = root
  && Tree.lca tree ~vert1:"G" ~vert2:"A" = "A"
  && Tree.lca tree ~vert1:"A" ~vert2:"G" = "A"
  && Tree.lca tree ~vert1:"F" ~vert2:"D" = "C"
  && Tree.lca tree ~vert1:"Main" ~vert2:"D" = root
  && Tree.lca tree ~vert1:"Main" ~vert2:"Main" = "Main"

let%test "find_out_edges" =
  let root_edges = Tree.find_out_edges tree root in
  let expected_edges =
    Hutil.set_from_list [ "Int"; "IO"; "String"; "Bool"; "A" ]
  in
  List.for_all ~f:(Hashtbl.mem expected_edges) root_edges
  && Tree.find_out_edges tree "F" = [ "G" ]
  && Tree.find_out_edges tree "E" = []

let%test "is_ancestor" =
  Tree.is_ancestor tree ~ancestor:root root
  && Tree.is_ancestor tree ~ancestor:"A" "A"
  && Tree.is_ancestor tree ~ancestor:"Int" "Int"
  && Tree.is_ancestor tree ~ancestor:"Int" "A" |> not
  && Tree.is_ancestor tree ~ancestor:"A" "Int" |> not
  && Tree.is_ancestor tree ~ancestor:"G" "A" |> not
  && Tree.is_ancestor tree ~ancestor:"A" "G"
  && Tree.is_ancestor tree ~ancestor:"F" "D" |> not
  && Tree.is_ancestor tree ~ancestor:"F" "B" |> not
  && Tree.is_ancestor tree ~ancestor:"B" "F"
  && Tree.is_ancestor tree ~ancestor:"Main" "D" |> not
  && Tree.is_ancestor tree ~ancestor:"Main" "Main"
