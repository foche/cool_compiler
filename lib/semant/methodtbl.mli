(* methodtbl.mli *)

open Parser
open Util
open Tables

type method_sig = {
    return_type : type_sym;
    formals : Ast.formal list;
    impl_class : type_sym;
    label : id_sym;
  }

type t

val create : int -> t

val add :
  tbl : t ->
  clazz : type_sym ->
  method_id : id_sym ->
  return_type : type_sym ->
  formals : Ast.formal list ->
  bool

val find_opt :
  tbl : t ->
  graph : type_sym Tree.t ->
  clazz : type_sym ->
  method_id : id_sym ->
  method_sig option

val iter : f : (type_sym -> id_sym -> method_sig -> unit) -> tbl : t -> unit

val for_all : f : (type_sym -> id_sym -> method_sig -> bool) -> tbl : t -> bool
