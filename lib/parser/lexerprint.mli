(* lexerprint.mli *)

open! StdLabels

val token_string : Coolparser.token -> String.t

val print_filename : String.t -> Unit.t
(** [print_filename filename] prints the name of the file [filename]. *)

val print_token : Coolparser.token -> Int.t -> Unit.t
(** [print_token tok n] prints the given token [tok] and line number [n]. *)
