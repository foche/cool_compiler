(* lexerprint.ml *)

open Util

let print_filename filename =
  filename |> Tbl.find Tables.str_const_tbl |> Printf.printf "#name %S\n"

let token_string tok =
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
  | Coolparser.STR_CONST s -> Printf.sprintf "STR_CONST %S" s
  | Coolparser.INT_CONST s -> Printf.sprintf "INT_CONST %s" s
  | Coolparser.TYPEID s -> Printf.sprintf "TYPEID %s" s
  | Coolparser.OBJECTID s -> Printf.sprintf "OBJECTID %s" s
  | Coolparser.BOOL_CONST x -> Printf.sprintf "BOOL_CONST %B" x
  | Coolparser.ERR err -> Printf.sprintf "ERROR %S" err

let print_token tok line_number =
  token_string tok |> Printf.printf "#%d %s\n" line_number
