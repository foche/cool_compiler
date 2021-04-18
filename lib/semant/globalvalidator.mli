(* globalvalidator.mli *)

open Parser
open Util

type validator_args = {
  program : Abstractsyntax.program;
  handle_to_class : (Tables.type_sym, Abstractsyntax.class_node) Hashtbl.t;
  graph : (Tables.type_sym, Tables.type_sym) Hashtbl.t;
  sigs : Methodtbl.t;
}

val validate : args:validator_args -> bool * Tables.type_sym Tree.t option
