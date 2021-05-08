(* tables.ml *)

open! StdLabels
module Hutil = Hashtblutil

type cool_id

type cool_typ

type cool_str

type cool_int

type id_sym = cool_id Strtbl.handle

type typ_sym = cool_typ Strtbl.handle

type str_sym = cool_str Strtbl.handle

type int_sym = cool_int Strtbl.handle

type arg = id_sym * typ_sym

let id_tbl = Strtbl.create 257

let type_tbl = Strtbl.create 61

let str_const_tbl = Strtbl.create 131

let int_const_tbl = Strtbl.create 61

let make_id id = Strtbl.add id_tbl id

let make_type typ = Strtbl.add type_tbl typ

let make_str str = Strtbl.add str_const_tbl str

let make_int str = Strtbl.add int_const_tbl str

let find_id = Strtbl.find id_tbl

let find_type = Strtbl.find type_tbl

let find_str = Strtbl.find str_const_tbl

let find_int = Strtbl.find int_const_tbl

let print_id ppf id = find_id id |> Format.fprintf ppf "%s"

let print_type ppf typ = find_type typ |> Format.fprintf ppf "%s"

let print_str ppf s = find_str s |> Format.fprintf ppf "%S"

let print_int ppf x = find_int x |> Format.fprintf ppf "%s"

let id_module = Strtbl.handle_module id_tbl

let typ_module = Strtbl.handle_module type_tbl

let int_module = Strtbl.handle_module int_const_tbl

let str_module = Strtbl.handle_module str_const_tbl

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

let basic_methods =
  [
    (object_type, obj_abort, object_type, []);
    (object_type, obj_type_name, string_type, []);
    (object_type, obj_copy, self_type, []);
    (io_type, io_out_str, self_type, [ (make_id "x", string_type) ]);
    (io_type, io_out_int, self_type, [ (make_id "x", int_type) ]);
    (io_type, io_in_str, string_type, []);
    (io_type, io_in_int, int_type, []);
    (string_type, str_len, int_type, []);
    (string_type, str_concat, string_type, [ (make_id "s", string_type) ]);
    ( string_type,
      str_substr,
      string_type,
      [ (make_id "i", int_type); (make_id "l", int_type) ] );
  ]

let reserved_classes =
  Hutil.init
    [
      (object_type, ());
      (io_type, ());
      (int_type, ());
      (string_type, ());
      (bool_type, ());
      (self_type, ());
    ]

let inheritance_blocklist =
  Hutil.init
    [ (int_type, ()); (string_type, ()); (bool_type, ()); (self_type, ()) ]

let primitives = [ int_type; string_type; bool_type ]

let id_count _ = Strtbl.length id_tbl

let basic_classes = io_type :: self_type :: primitives

let method_label typ method_id =
  Printf.sprintf "%s.%s" (find_type typ) (find_id method_id) |> make_id

let is_primitive = List.mem ~set:primitives
