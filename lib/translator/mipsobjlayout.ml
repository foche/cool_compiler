(* mipsobjlayout.ml *)

open MoreLabels
open Util

type access = int

type method_info = { offset : access; label : Temp.label }

type t = {
  typ : Tables.type_sym;
  field_offsets : (Tables.id_sym, access) Hashtbl.t;
  method_info : (Tables.id_sym, method_info) Hashtbl.t;
  curr_field_offset : access ref;
  curr_method_offset : access ref;
}

let word_size = 4

let obj_header_size = 3 * word_size

let create ~typ =
  {
    typ;
    field_offsets = Hashtbl.create 31;
    method_info = Hashtbl.create 31;
    curr_field_offset = ref obj_header_size;
    curr_method_offset = ref 0;
  }

let copy other ~typ =
  {
    typ;
    field_offsets = Hashtbl.copy other.field_offsets;
    method_info = Hashtbl.copy other.method_info;
    curr_field_offset = ref !(other.curr_field_offset);
    curr_method_offset = ref !(other.curr_method_offset);
  }

let alloc_field obj field_name ~size =
  let offset = !(obj.curr_field_offset) in
  Hashtbl.add obj.field_offsets ~key:field_name ~data:offset;
  obj.curr_field_offset := offset + size;
  offset

let alloc_method obj method_id ~label =
  let offset = !(obj.curr_method_offset) in
  Hashtbl.add obj.method_info ~key:method_id ~data:{ offset; label };
  obj.curr_method_offset := offset + word_size;
  offset

let access_field obj = Hashtbl.find obj.field_offsets

let access_method obj method_id =
  (Hashtbl.find obj.method_info method_id).offset
