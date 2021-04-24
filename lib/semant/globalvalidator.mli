(* globalvalidator.mli *)

open Parser
open Util

type validator_args = {
  program : Abstractsyntax.program;
  handle_to_class : (Tables.typ_sym, Abstractsyntax.class_node) Hashtbl.t;
  parents : (Tables.typ_sym, Tables.typ_sym) Hashtbl.t;
  sigs : Methodtbl.t;
}

val validate : args:validator_args -> bool * Tables.typ_sym Tree.t option
