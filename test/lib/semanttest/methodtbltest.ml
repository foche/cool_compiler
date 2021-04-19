(* methodtbltest.ml *)

open Semant
open Util
open Testutil
module Tutil = Treeutil

let foo_method = Tables.make_id "foo"

let bar_method = Tables.make_id "bar"

let x = Tables.make_id "x"

let y = Tables.make_id "y"

let%test "Duplicate add" =
  let tbl = Methodtbl.create 31 in
  Methodtbl.add tbl ~typ:Tutil.cl_a ~method_id:foo_method
    ~ret_typ:Tables.int_type ~formals:[]
  && Methodtbl.add tbl ~typ:Tutil.cl_a ~method_id:foo_method ~ret_typ:Tutil.cl_b
       ~formals:[ (x, Tables.bool_type) ]
     |> not
  && Methodtbl.add tbl ~typ:Tutil.cl_b ~method_id:foo_method
       ~ret_typ:Tables.int_type ~formals:[]

let%test "find_opt" =
  let tbl = Methodtbl.create 31 in
  let formals = [ (x, Tables.int_type); (y, Tables.bool_type) ] in
  let label_a_foo = Tables.method_label Tutil.cl_a foo_method in
  let label_c_foo = Tables.method_label Tutil.cl_c foo_method in
  let cl_a_foo_sig =
    {
      Methodtbl.ret_typ = Tutil.cl_b;
      formals;
      impl_class = Tutil.cl_a;
      label = label_a_foo;
    }
  in
  let cl_c_foo_sig =
    { cl_a_foo_sig with Methodtbl.impl_class = Tutil.cl_c; label = label_c_foo }
  in
  (* inheritance path: D -> C -> B -> A *)
  Methodtbl.add tbl ~typ:Tutil.cl_a ~method_id:foo_method ~ret_typ:Tutil.cl_b
    ~formals
  && Methodtbl.add tbl ~typ:Tutil.cl_b ~method_id:bar_method
       ~ret_typ:Tables.int_type ~formals
  && Methodtbl.add tbl ~typ:Tutil.cl_c ~method_id:foo_method ~ret_typ:Tutil.cl_b
       ~formals
  && Methodtbl.find_opt tbl ~inherit_tree:Treeutil.tree ~typ:Tutil.cl_c
       ~method_id:foo_method
     = Some cl_c_foo_sig
  && Methodtbl.find_opt tbl ~inherit_tree:Treeutil.tree ~typ:Tutil.cl_d
       ~method_id:foo_method
     = Some cl_c_foo_sig
  && Methodtbl.find_opt tbl ~inherit_tree:Treeutil.tree ~typ:Tutil.cl_a
       ~method_id:foo_method
     = Some cl_a_foo_sig
  && Methodtbl.find_opt tbl ~inherit_tree:Treeutil.tree ~typ:Tutil.cl_a
       ~method_id:bar_method
     = None
  && Methodtbl.find_opt tbl ~inherit_tree:Treeutil.tree ~typ:Tutil.cl_b
       ~method_id:foo_method
     = Some cl_a_foo_sig
