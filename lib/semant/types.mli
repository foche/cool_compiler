(* types.mli *)

open Util
module T = Tables

val translate_type : cl_typ:T.type_sym -> T.type_sym -> T.type_sym

val is_subtype : T.type_sym Tree.t -> cl_typ:T.type_sym -> sub_typ:T.type_sym -> super_typ:T.type_sym -> bool
