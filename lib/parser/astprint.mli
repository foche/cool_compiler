(* astprint.mli *)

val print_ast : Ast.program -> unit
(** [print_ast ast] pretty-prints the [ast]. *)

val print_syntax_error : unit -> unit
(** [print_syntax_error ()] prints a syntax error message to stderr. *)

val print_eof_error : string -> unit
(**
  [print_eof_error filename] prints a syntax error for an empty program
  with the given [filename].
 *)

val err_unclosed : string -> Lexing.position -> Lexing.position -> unit

val err_expected : string -> Lexing.position -> Lexing.position -> unit

val err_syntax : Lexing.position -> Lexing.position -> unit
