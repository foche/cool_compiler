(* globalvalidator.mli *)

open Parser
open Util

type validator_args = {
    program : Ast.program;
    reserved_classes : (Ast.type_sym, unit) Hashtbl.t;
    inheritance_blocklist : (Ast.type_sym, unit) Hashtbl.t;
    handle_to_class : (Ast.type_sym, Ast.class_node) Hashtbl.t;
    graph : (Ast.type_sym, Ast.type_sym) Hashtbl.t;
    sigs : Methodtbl.t;
  }

val validate : validator_args -> bool * Ast.type_sym Tree.t option
