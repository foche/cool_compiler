(* semantprint.mli *)

open Parser

val print_location : Ast.str_sym -> int -> unit

val print_typecheck_error : unit -> unit
(** [print_typecheck_error ()] prints a typechecking error message to [stderr]. *)
