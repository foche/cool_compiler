(* tables.mli *)

open! MoreLabels
open! StdLabels

type id_sym

type typ_sym

type str_sym

type int_sym

val make_id : String.t -> id_sym

val make_type : String.t -> typ_sym

val make_str : String.t -> str_sym

val make_int : String.t -> int_sym

val find_id : id_sym -> String.t

val find_type : typ_sym -> String.t

val find_str : str_sym -> String.t

val find_int : int_sym -> String.t

val print_id : Format.formatter -> id_sym -> Unit.t

val print_type : Format.formatter -> typ_sym -> Unit.t

val print_str : Format.formatter -> str_sym -> Unit.t

val print_int : Format.formatter -> int_sym -> Unit.t

val empty_str : str_sym

val object_type : typ_sym

val io_type : typ_sym

val int_type : typ_sym

val string_type : typ_sym

val bool_type : typ_sym

val self_var : id_sym

val self_type : typ_sym

val main_method : id_sym

val main_type : typ_sym

val obj_abort : id_sym

val obj_type_name : id_sym

val obj_copy : id_sym

val io_out_str : id_sym

val io_out_int : id_sym

val io_in_str : id_sym

val io_in_int : id_sym

val str_len : id_sym

val str_concat : id_sym

val str_substr : id_sym

type arg = id_sym * typ_sym

val basic_methods : (typ_sym * id_sym * typ_sym * arg List.t) List.t

val reserved_classes : (typ_sym, Unit.t) Hashtbl.t

val inheritance_blocklist : (typ_sym, Unit.t) Hashtbl.t

val id_count : 'a -> Int.t

val basic_classes : typ_sym List.t

val is_primitive : typ_sym -> Bool.t

val method_label : typ_sym -> id_sym -> id_sym
