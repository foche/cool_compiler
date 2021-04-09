(* localvalidator.mli *)

open Parser
open Util

type validator_args = {
    ignored_classes : (Ast.type_sym, unit) Hashtbl.t;
    id_env : (Ast.id_sym, Ast.type_sym) Typeenv.t;
    func_env : (Ast.id_sym, Ast.mthd) Typeenv.t;
    graph : Ast.type_sym Tree.t;
    sigs : Methodtbl.t;
    untyped_classes : (Ast.type_sym, Ast.class_node) Hashtbl.t;
    typed_classes : (Ast.type_sym, Ast.class_node) Hashtbl.t;
  }

val validate : validator_args -> bool
