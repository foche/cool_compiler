(* typecheck.mli *)

open Parser
(* open Translator *)

(* module type Typechecker =
  sig *)
(* type layout *)

val semant_verbose : bool ref
(** [semant_verbose] is a flag that enables AST printing after typechecking. *)

val typecheck : Abstractsyntax.program -> Abstractsyntax.program option
(**
      [typecheck ast] returns [Some ast'] if [ast] can be typechecked, where [ast']
      is [ast] annotated with types. Otherwise returns [None].
     *)
(* end *)

(* module Make (Obj : Objectlayout.Layout) : Typechecker with type layout = Obj.t *)
