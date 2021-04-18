(* lexerprint.mli *)

val token_string : Coolparser.token -> string

val print_filename : string -> unit
(** [print_filename filename] prints the name of the file [filename]. *)

val print_token : Coolparser.token -> int -> unit
(** [print_token tok n] prints the given token [tok] and line number [n]. *)
