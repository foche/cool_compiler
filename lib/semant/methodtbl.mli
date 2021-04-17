(* methodtbl.mli *)

open Parser
open Util

type method_sig =
  { return_type: Tables.type_sym
  ; formals: Abstractsyntax.formal list
  ; impl_class: Tables.type_sym
  ; label: Tables.id_sym }

type t

val create : int -> t

val add :
     tbl:t
  -> cl:Tables.type_sym
  -> method_id:Tables.id_sym
  -> return_type:Tables.type_sym
  -> formals:Abstractsyntax.formal list
  -> bool

val find_opt :
     tbl:t
  -> graph:Tables.type_sym Tree.t
  -> cl:Tables.type_sym
  -> method_id:Tables.id_sym
  -> method_sig option

val iter : f:(Tables.type_sym -> Tables.id_sym -> method_sig -> unit) -> tbl:t -> unit

val for_all : f:(Tables.type_sym -> Tables.id_sym -> method_sig -> bool) -> tbl:t -> bool
