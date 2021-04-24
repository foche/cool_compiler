(* tables.mli *)

(* open StdLabels *)
open MoreLabels

type id_sym

type typ_sym

type str_sym

type int_sym

val make_id : string -> id_sym

val make_type : string -> typ_sym

val make_str : string -> str_sym

val make_int : string -> int_sym

val find_id : id_sym -> string

val find_type : typ_sym -> string

val find_str : str_sym -> string

val find_int : int_sym -> string

val print_id : Format.formatter -> id_sym -> unit

val print_type : Format.formatter -> typ_sym -> unit

val print_str : Format.formatter -> str_sym -> unit

val print_int : Format.formatter -> int_sym -> unit

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

val basic_methods : (typ_sym * id_sym * typ_sym * arg list) list

val reserved_classes : (typ_sym, unit) Hashtbl.t

val inheritance_blocklist : (typ_sym, unit) Hashtbl.t

(* val primitives : typ_sym list *)

val id_count : 'a -> int

val basic_classes : typ_sym list

(* val create_basic_labels _ =
  val tbl = Hashtbl.create 32 in
  List.iter
    ~f:(fun (typ, method_id, _, _) ->
      Hashtbl.add tbl ~key:(typ, method_id) ~data:(method_label typ method_id))
    basic_methods;
  tbl

val basic_method_labels = create_basic_labels () *)

val is_primitive : typ_sym -> bool

val method_label : typ_sym -> id_sym -> id_sym
