(* tables.ml *)

let id_tbl = Tbl.create 128
let type_tbl = Tbl.create 128
let str_const_tbl = Tbl.create 128
let int_const_tbl = Tbl.create 128

let make_id id =
  Tbl.add id_tbl id

let make_type typ =
  Tbl.add type_tbl typ

let object_type = make_type "Object"
let io_type = make_type "IO"
let int_type = make_type "Int"
let string_type = make_type "String"
let bool_type = make_type "Bool"
let self_var = make_id "self"
let self_type = make_type "SELF_TYPE"
let main_method = make_id "main"
let main_type = make_type "Main"

let print_id out id =
  Tbl.find id_tbl id |> Printf.fprintf out "%s"

let print_type out typ =
  Tbl.find type_tbl typ |> Printf.fprintf out "%s"

let print_str out s =
  Tbl.find str_const_tbl s |> Printf.fprintf out "%s"

let print_int out x =
  Tbl.find int_const_tbl x |> Printf.fprintf out "%s"
