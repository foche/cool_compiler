(* parserexpecttest.ml *)

open Parser

let _ =
  Parsedriver.parser_verbose := true;
  Parsedriver.parse [ Sys.argv.(1) ]
