(* astprint.mli *)

val print_ast : Ast.program -> unit
(** [print_ast ast] pretty-prints the [ast]. *)

val print_syntax_error : unit -> unit
(** [print_syntax_error ()] prints a syntax error message to stderr. *)

val print_error_item : Lexing.position -> Lexing.position -> unit
(**
  [print_error_item pos_start pos_end]
  prints a syntax error message given location from [pos_start] to [pos_end].
 *)

val print_eof_error : string -> unit
(**
  [print_eof_error filename] prints a syntax error for an empty program
  with the given [filename].
 *)
