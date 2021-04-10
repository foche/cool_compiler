(* tables.ml *)

open Helpers

type id_sym = Tbl.handle
type type_sym = Tbl.handle
type str_sym = Tbl.handle
type int_sym = Tbl.handle

let id_tbl = Tbl.create 128
let type_tbl = Tbl.create 128
let str_const_tbl = Tbl.create 128
let int_const_tbl = Tbl.create 128

let make_id id =
  Tbl.add id_tbl id

let make_type typ =
  Tbl.add type_tbl typ

let make_str str =
  Tbl.add str_const_tbl str

let find_id = Tbl.find id_tbl
let find_type = Tbl.find type_tbl
let find_str = Tbl.find str_const_tbl
let find_int = Tbl.find int_const_tbl

let print_id out id =
  find_id id |> Printf.fprintf out "%s"

let print_type out typ =
  find_type typ |> Printf.fprintf out "%s"

let print_str out s =
  find_str s |> Printf.fprintf out "%s"

let print_int out x =
  find_int x |> Printf.fprintf out "%s"

let method_label typ method_id =
  Printf.sprintf "%s.%s"
    (find_type typ)
    (find_id method_id) |>
  make_id

let clinit_label typ =
  find_type typ |> Printf.sprintf "%s_init" |> make_id

let prototype_label typ =
  find_type typ |> Printf.sprintf "%s_protObj" |> make_id

let empty_str = make_str ""

let object_type = make_type "Object"
let io_type = make_type "IO"
let int_type = make_type "Int"
let string_type = make_type "String"
let bool_type = make_type "Bool"
let self_var = make_id "self"
let self_type = make_type "SELF_TYPE"
let main_method = make_id "main"
let main_type = make_type "Main"

let obj_abort = make_id "abort"
let obj_type_name = make_id "type_name"
let obj_copy = make_id "copy"
let io_out_str = make_id "out_string"
let io_out_int = make_id "out_int"
let io_in_str = make_id "in_string"
let io_in_int = make_id "in_int"
let str_len = make_id "length"
let str_concat = make_id "concat"
let str_substr = make_id "substr"

let basic_methods = [
    object_type, obj_abort, object_type, [];
    object_type, obj_type_name, string_type, [];
    object_type, obj_copy, self_type, [];
    io_type, io_out_str, self_type, [make_id "x", string_type];
    io_type, io_out_int, self_type, [make_id "x", int_type];
    io_type, io_in_str, string_type, [];
    io_type, io_in_int, int_type, [];
    string_type, str_len, int_type, [];
    string_type, str_concat, string_type, [make_id "s", string_type];
    string_type, str_substr, string_type, [make_id "i", int_type; make_id "l", int_type];
  ]

let reserved_classes = init_hashtbl 16 [
    object_type, ();
    io_type, ();
    int_type, ();
    string_type, ();
    bool_type, ();
    self_type, ();
  ]

let inheritance_blocklist = init_hashtbl 8 [
    int_type, ();
    string_type, ();
    bool_type, ();
    self_type, ();
  ]

let primitives = [
    int_type;
    string_type;
    bool_type;
  ]

let basic_classes = io_type :: self_type :: primitives

let create_basic_labels _ =
  let tbl = Hashtbl.create 32 in
  List.iter (fun (typ, method_id, _, _) ->
      method_label typ method_id |> Hashtbl.replace tbl (typ, method_id))
    basic_methods;
  tbl

let basic_method_labels = create_basic_labels ()

let is_prim typ =
  List.find_opt ((=) typ) primitives |> is_some_opt
