(* methodtbl.mli *)

open Parser
open Util

type method_sig = {
    return_type : Ast.type_sym;
    formals : Ast.formal list;
  }

type t

val create : int -> t

val add :
  tbl : t ->
  clazz : Ast.type_sym ->
  method_id : Ast.id_sym ->
  return_type : Ast.type_sym ->
  formals : Ast.formal list ->
  bool

val find_opt :
  tbl : t ->
  graph : Ast.type_sym Tree.t ->
  clazz : Ast.type_sym ->
  method_id : Ast.id_sym ->
  method_sig option

val iter : f : (Ast.type_sym -> Ast.id_sym -> method_sig -> unit) -> tbl : t -> unit

val for_all : f : (Ast.type_sym -> Ast.id_sym -> method_sig -> bool) -> tbl : t -> bool
