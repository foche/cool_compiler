(* tcheck.mli *)

open Parser

val semant_verbose : bool ref
(** [semant_verbose] is a flag that enables AST printing after typechecking. *)

val typecheck : Ast.program -> Ast.program option
(**
  [typecheck ast] returns [Some ast'] if [ast] can be typechecked, where [ast']
  is [ast] annotated with types. Otherwise returns [None].
 *)
