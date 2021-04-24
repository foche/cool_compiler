(* lexerprint.ml *)

module Parse = Coolparser

let print_filename = Format.printf "#name %S@."

let token_string (tok : Parse.token) =
  match tok with
  | EOF -> "EOF"
  | CLASS -> "CLASS"
  | ELSE -> "ELSE"
  | FI -> "FI"
  | IF -> "IF"
  | IN -> "IN"
  | INHERITS -> "INHERITS"
  | LET -> "LET"
  | LOOP -> "LOOP"
  | POOL -> "POOL"
  | THEN -> "THEN"
  | WHILE -> "WHILE"
  | CASE -> "CASE"
  | ESAC -> "ESAC"
  | OF -> "OF"
  | DARROW -> "DARROW"
  | NEW -> "NEW"
  | ISVOID -> "ISVOID"
  | ASSIGN -> "ASSIGN"
  | NOT -> "NOT"
  | LE -> "LE"
  | LT -> "'<'"
  | EQ -> "'='"
  | PLUS -> "'+'"
  | MINUS -> "'-'"
  | MULT -> "'*'"
  | DIV -> "'/'"
  | NEG -> "'~'"
  | AT -> "'@'"
  | DOT -> "'.'"
  | COMMA -> "','"
  | COLON -> "':'"
  | SEMI -> "';'"
  | LBRACE -> "'{'"
  | RBRACE -> "'}'"
  | LPAREN -> "'('"
  | RPAREN -> "')'"
  | STR_CONST s -> Printf.sprintf "STR_CONST %S" s
  | INT_CONST s -> Printf.sprintf "INT_CONST %s" s
  | TYPEID s -> Printf.sprintf "TYPEID %s" s
  | OBJECTID s -> Printf.sprintf "OBJECTID %s" s
  | BOOL_CONST x -> Printf.sprintf "BOOL_CONST %B" x
  | ERR err -> Printf.sprintf "ERROR %S" err

let print_token tok line_number =
  token_string tok |> Format.printf "#%d %s@." line_number
