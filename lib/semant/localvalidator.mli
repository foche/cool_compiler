(* localvalidator.mli *)

open Parser
open Util

type validator_args =
  { ignored_classes: (Tables.type_sym, unit) Hashtbl.t
  ; id_env: (Tables.id_sym, Tables.type_sym) Symtbl.t
  ; func_env: (Tables.id_sym, Ast.mthd) Symtbl.t
  ; graph: Tables.type_sym Tree.t
  ; sigs: Methodtbl.t
  ; untyped_classes: (Tables.type_sym, Ast.class_node) Hashtbl.t
  ; typed_classes: (Tables.type_sym, Ast.class_node) Hashtbl.t }

val validate : validator_args -> bool
