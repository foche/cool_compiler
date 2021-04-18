(* semantprint.mli *)

val print_location : Lexing.position * Lexing.position -> unit

val print_typecheck_error : unit -> unit
(** [print_typecheck_error ()] prints a typechecking error message to [stderr]. *)
