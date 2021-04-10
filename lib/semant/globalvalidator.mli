(* globalvalidator.mli *)

open Parser
open Util

type validator_args = {
    program : Ast.program;
    reserved_classes : (Tables.type_sym, unit) Hashtbl.t;
    inheritance_blocklist : (Tables.type_sym, unit) Hashtbl.t;
    handle_to_class : (Tables.type_sym, Ast.class_node) Hashtbl.t;
    graph : (Tables.type_sym, Tables.type_sym) Hashtbl.t;
    sigs : Methodtbl.t;
  }

val validate : validator_args -> bool * Tables.type_sym Tree.t option
