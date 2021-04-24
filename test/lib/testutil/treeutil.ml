(* treeutil.ml *)

open MoreLabels
open Util
module T = Tables

let root = T.object_type

let cl_a = T.make_type "A"

let cl_b = T.make_type "B"

let cl_c = T.make_type "C"

let cl_d = T.make_type "D"

let cl_e = T.make_type "E"

let cl_f = T.make_type "F"

let cl_g = T.make_type "G"

(*
  Inheritance tree:

    Object ------------------
      |  \                   \
      A  [Int; String; Bool] IO
      |                       \
      B                      Main
     / \
    C   E
   / \
  D   F
       \
        G
 *)
let create_tree_parents _ =
  Hashtblutil.init
    [
      (T.int_type, root);
      (T.io_type, root);
      (T.string_type, root);
      (T.bool_type, root);
      (T.main_type, T.io_type);
      (cl_a, root);
      (cl_b, cl_a);
      (cl_c, cl_b);
      (cl_d, cl_c);
      (cl_e, cl_b);
      (cl_f, cl_c);
      (cl_g, cl_f);
    ]

let tree =
  let parents = create_tree_parents () in
  match Tree.create ~parents ~root with
  | Tree tree ->
      Hashtbl.add parents ~key:root ~data:root;
      (* should not affect anything *)
      tree
  | _ -> failwith "Could not create tree"
