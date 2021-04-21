(* lexerexpecttest.ml *)

open Parser

let rec test_lexer buf =
  match Coollexer.get_token buf with
  | Coolparser.EOF -> ()
  | _ -> test_lexer buf

let _ =
  let filename = Sys.argv.(1) in
  let file = open_in filename in
  Coollexer.lexer_debug := true;
  Coollexer.print_filename filename;
  (try Lexing.from_channel file |> test_lexer
   with exn ->
     close_in file;
     raise exn);
  close_in file
