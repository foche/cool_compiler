(* lexerprint.ml *)

open Util

let print_filename filename =
  filename |> Tbl.find Tables.str_const_tbl |> Printf.printf "#name %S\n"

let print_token tok line_number =
  let token_string =
    match tok with
    | Coolparser.EOF -> "EOF"
    | Coolparser.CLASS -> "CLASS"
    | Coolparser.ELSE -> "ELSE"
    | Coolparser.FI -> "FI"
    | Coolparser.IF -> "IF"
    | Coolparser.IN -> "IN"
    | Coolparser.INHERITS -> "INHERITS"
    | Coolparser.LET -> "LET"
    | Coolparser.LOOP -> "LOOP"
    | Coolparser.POOL -> "POOL"
    | Coolparser.THEN -> "THEN"
    | Coolparser.WHILE -> "WHILE"
    | Coolparser.CASE -> "CASE"
    | Coolparser.ESAC -> "ESAC"
    | Coolparser.OF -> "OF"
    | Coolparser.DARROW -> "DARROW"
    | Coolparser.NEW -> "NEW"
    | Coolparser.ISVOID -> "ISVOID"
    | Coolparser.ASSIGN -> "ASSIGN"
    | Coolparser.NOT -> "NOT"
    | Coolparser.LE -> "LE"
    | Coolparser.LT -> "'<'"
    | Coolparser.EQ -> "'='"
    | Coolparser.PLUS -> "'+'"
    | Coolparser.MINUS -> "'-'"
    | Coolparser.MULT -> "'*'"
    | Coolparser.DIV -> "'/'"
    | Coolparser.NEG -> "'~'"
    | Coolparser.AT -> "'@'"
    | Coolparser.DOT -> "'.'"
    | Coolparser.COMMA -> "','"
    | Coolparser.COLON -> "':'"
    | Coolparser.SEMI -> "';'"
    | Coolparser.LBRACE -> "'{'"
    | Coolparser.RBRACE -> "'}'"
    | Coolparser.LPAREN -> "'('"
    | Coolparser.RPAREN -> "')'"
    | Coolparser.STR_CONST handle ->
      Tbl.find Tables.str_const_tbl handle |> Printf.sprintf "STR_CONST %S"
    | Coolparser.INT_CONST handle ->
      Tbl.find Tables.int_const_tbl handle |> Printf.sprintf "INT_CONST %s"
    | Coolparser.TYPEID handle ->
      Tbl.find Tables.type_tbl handle |> Printf.sprintf "TYPEID %s"
    | Coolparser.OBJECTID handle ->
      Tbl.find Tables.id_tbl handle |> Printf.sprintf "OBJECTID %s"
    | Coolparser.BOOL_CONST x -> Printf.sprintf "BOOL_CONST %B" x
    | Coolparser.ERR err -> Printf.sprintf "ERROR %S" err in
  Printf.printf "#%d %s\n" line_number token_string
