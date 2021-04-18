(* lexerprint.ml *)

module Parse = Coolparser

let print_filename filename = Printf.printf "#name %S\n" filename

let token_string tok =
  match tok with
  | Parse.EOF -> "EOF"
  | Parse.CLASS -> "CLASS"
  | Parse.ELSE -> "ELSE"
  | Parse.FI -> "FI"
  | Parse.IF -> "IF"
  | Parse.IN -> "IN"
  | Parse.INHERITS -> "INHERITS"
  | Parse.LET -> "LET"
  | Parse.LOOP -> "LOOP"
  | Parse.POOL -> "POOL"
  | Parse.THEN -> "THEN"
  | Parse.WHILE -> "WHILE"
  | Parse.CASE -> "CASE"
  | Parse.ESAC -> "ESAC"
  | Parse.OF -> "OF"
  | Parse.DARROW -> "DARROW"
  | Parse.NEW -> "NEW"
  | Parse.ISVOID -> "ISVOID"
  | Parse.ASSIGN -> "ASSIGN"
  | Parse.NOT -> "NOT"
  | Parse.LE -> "LE"
  | Parse.LT -> "'<'"
  | Parse.EQ -> "'='"
  | Parse.PLUS -> "'+'"
  | Parse.MINUS -> "'-'"
  | Parse.MULT -> "'*'"
  | Parse.DIV -> "'/'"
  | Parse.NEG -> "'~'"
  | Parse.AT -> "'@'"
  | Parse.DOT -> "'.'"
  | Parse.COMMA -> "','"
  | Parse.COLON -> "':'"
  | Parse.SEMI -> "';'"
  | Parse.LBRACE -> "'{'"
  | Parse.RBRACE -> "'}'"
  | Parse.LPAREN -> "'('"
  | Parse.RPAREN -> "')'"
  | Parse.STR_CONST s -> Printf.sprintf "STR_CONST %S" s
  | Parse.INT_CONST s -> Printf.sprintf "INT_CONST %s" s
  | Parse.TYPEID s -> Printf.sprintf "TYPEID %s" s
  | Parse.OBJECTID s -> Printf.sprintf "OBJECTID %s" s
  | Parse.BOOL_CONST x -> Printf.sprintf "BOOL_CONST %B" x
  | Parse.ERR err -> Printf.sprintf "ERROR %S" err

let print_token tok line_number =
  token_string tok |> Printf.printf "#%d %s\n" line_number
