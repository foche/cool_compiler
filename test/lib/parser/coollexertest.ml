(** parsertests.ml *)

open Parser
open Coolparser

let assert_equal_tokens s toks =
  let ic = Lexing.from_string s in
  List.for_all (fun tok -> tok = Coollexer.next_token ic) toks &&
  EOF = Coollexer.next_token ic

let assert_equal_token s tok =
  assert_equal_tokens s [tok]

let stringify s =
  "\"" ^ s ^ "\""

let%test _ = assert_equal_token "class" CLASS
let%test _ = assert_equal_token "if" IF
let%test _ = assert_equal_token "iff" (OBJECTID "iff")
let%test _ = assert_equal_token "iF" IF
let%test _ = assert_equal_token "If" IF
let%test _ = assert_equal_token "IF" IF
let%test _ = assert_equal_token "then" THEN
let%test _ = assert_equal_token "tHEn" THEN
let%test _ = assert_equal_token "Then" THEN
let%test _ = assert_equal_token "else" ELSE
let%test _ = assert_equal_token "fi" FI
let%test _ = assert_equal_token "inherits" INHERITS
let%test _ = assert_equal_token "isvoid" ISVOID
let%test _ = assert_equal_token "let" LET
let%test _ = assert_equal_token "in" IN
let%test _ = assert_equal_token "while" WHILE
let%test _ = assert_equal_token "loop" LOOP
let%test _ = assert_equal_token "pool" POOL
let%test _ = assert_equal_token "case" CASE
let%test _ = assert_equal_token "of" OF
let%test _ = assert_equal_token "esac" ESAC
let%test _ = assert_equal_token "new" NEW
let%test _ = assert_equal_token "not" NOT
let%test _ = assert_equal_token "true" (BOOL_CONST true)
let%test _ = assert_equal_token "false" (BOOL_CONST false)
let%test _ = assert_equal_token "tRUE" (BOOL_CONST true)
let%test _ = assert_equal_token "fAlsE" (BOOL_CONST false)
let%test _ = assert_equal_token "True" (TYPEID "True")
let%test _ = assert_equal_token "TrUe" (TYPEID "TrUe")
let%test _ = assert_equal_token "False" (TYPEID "False")

let%test _ = assert_equal_token "123" (INT_CONST "123")
let%test _ = assert_equal_token "0" (INT_CONST "0")
let%test _ = assert_equal_token "=>" DARROW
let%test _ = assert_equal_token "<=" LE
let%test _ = assert_equal_token "<" LT
let%test _ = assert_equal_token "=" EQ
let%test _ = assert_equal_token "+" PLUS
let%test _ = assert_equal_token "-" MINUS
let%test _ = assert_equal_token "*" MULT
let%test _ = assert_equal_token "/" DIV
let%test _ = assert_equal_token "~" NEG
let%test _ = assert_equal_token "." DOT
let%test _ = assert_equal_token "," COMMA
let%test _ = assert_equal_token ";" SEMI
let%test _ = assert_equal_token ":" COLON
let%test _ = assert_equal_token "@" AT
let%test _ = assert_equal_token "<-" ASSIGN
let%test _ = assert_equal_token "{" LBRACE
let%test _ = assert_equal_token "}" RBRACE
let%test _ = assert_equal_token "(" LPAREN
let%test _ = assert_equal_token ")" RPAREN
let%test _ = assert_equal_token "" EOF

let%test _ = assert_equal_token "(**)" EOF
let%test _ = assert_equal_token "(* -- *)" EOF
let%test _ = assert_equal_token "(* \000 \\ \t \r *)" EOF
let%test _ = assert_equal_token "(* 3 * 5 *)" EOF
let%test _ = assert_equal_token "(*\n\n*)" EOF
let%test _ = assert_equal_token "(*" (ERR "EOF in comment")
let%test _ = assert_equal_token "(*)" (ERR "EOF in comment")
let%test _ = assert_equal_token "(*(*)" (ERR "EOF in comment")
let%test _ = assert_equal_token "(" LPAREN
let%test _ = assert_equal_tokens "( *" [LPAREN; MULT]
let%test _ = assert_equal_token "(********)" EOF
let%test _ = assert_equal_token "(* (* **** *) (* (* *** *) *) *)" EOF
let%test _ = assert_equal_token "(* (* **** *) (* (* *** ) *) *)" (ERR "EOF in comment")
let%test _ = assert_equal_tokens "( (* **** *) (* (* *** *) *) *)" [LPAREN; ERR "Unmatched *)"]
let%test _ = assert_equal_token "(* \"hello\" *)" EOF
let%test _ = assert_equal_token "(* \"hello\" *)" EOF
let%test _ = assert_equal_token "--\"--\"--\"\n\"--\"--\"--\n--\"--\"--\"" (STR_CONST "--")
let%test _ =
  assert_equal_token
    "\"\\\" \\\\ \\0 \\b \\t \\n \\f \\\n \011\""
    (STR_CONST "\" \\ 0 \b \t \n \012 \n \011")
let%test _ = assert_equal_token "\"\\\"\\0\\\\0\\f\\\\f\"" (STR_CONST "\"0\\0\012\\f")
let%test _ = assert_equal_tokens "\"\000 4 * 4\" in" [ERR "String contains null character."; IN]
let%test _ = assert_equal_tokens "\"\000 4 * 4 \nin" [ERR "String contains null character."; IN]
let%test _ = assert_equal_token "\"" (ERR "EOF in string constant")
let%test _ = assert_equal_tokens "\"\n\"" [ERR "Unterminated string constant"; ERR "EOF in string constant"]
let%test _ = assert_equal_tokens "\"\\\\\n\"" [ERR "Unterminated string constant"; ERR "EOF in string constant"]
let%test _ = assert_equal_token "\"\\\\\\\n\"" (STR_CONST "\\\n")
let%test _ = assert_equal_tokens "_abc" [ERR "_"; OBJECTID "abc"]
let%test _ = assert_equal_token "abc_" (OBJECTID "abc_")
let%test _ = assert_equal_tokens "abc-123" [OBJECTID "abc"; MINUS; INT_CONST "123"]
let%test _ = assert_equal_token "abc_123" (OBJECTID "abc_123")
let%test _ = assert_equal_token "Abc_123" (TYPEID "Abc_123")
let%test _ = assert_equal_token "aBC_123" (OBJECTID "aBC_123")
let%test _ = assert_equal_token "-- \"123\"" EOF
let%test _ = assert_equal_token "--- 123" EOF
let%test _ = assert_equal_tokens "abc--\n 123" [OBJECTID "abc"; INT_CONST "123"]
let%test _ = assert_equal_token "(* * \"123\" -- * *) abc" (OBJECTID "abc")
let%test _ = assert_equal_token "-- (*\n*)" (ERR "Unmatched *)")
let%test _ = assert_equal_token "\" (* 123 *) \"" (STR_CONST " (* 123 *) ")
let%test _ = assert_equal_token "\" -- 123 \"" (STR_CONST " -- 123 ")
let%test _ = assert_equal_token "(* \n * 123 * \n *)" EOF
let%test _ = assert_equal_token "\"\\" (ERR "EOF in string constant")
let%test _ = assert_equal_token "\"hello\\\"" (ERR "EOF in string constant")
let%test _ = assert_equal_token "\"hello\\\nqweqw" (ERR "EOF in string constant")
let%test _ =
  assert_equal_tokens
    "(*\"(*\"*)\"*)\"(*\"*)\"*)(*\""
    [STR_CONST "(*"; ERR "Unmatched *)"; STR_CONST "*)(*"]
let%test _ = assert_equal_token "(*\n\n*)" EOF
let%test _ = assert_equal_token "000" (INT_CONST "000")
let%test _ = assert_equal_tokens "0x14" [INT_CONST "0"; OBJECTID "x14"]
let%test _ = assert_equal_tokens "-14" [MINUS; INT_CONST "14"]
let%test _ = assert_equal_tokens "~14" [NEG; INT_CONST "14"]

let long_int = String.make 5000 '9'
let%test _ = assert_equal_token long_int (INT_CONST long_int)
let%test _ =
  assert_equal_tokens
    "!#$%^&_>?`[]\\|'"
    [ERR "!"; ERR "#"; ERR "$"; ERR "%"; ERR "^"; ERR "&"; ERR "_"; ERR ">"; ERR "?";
     ERR "`"; ERR "["; ERR "]"; ERR "\\"; ERR "|"; ERR "'"]
let%test _ = assert_equal_tokens "" [ERR "\001"; ERR "\002"; ERR "\003"; ERR "\004"]
let%test _ = assert_equal_tokens "<<=" [LT; LE]
let%test _ = assert_equal_tokens "<==" [LE; EQ]
let%test _ = assert_equal_tokens "<==>" [LE; DARROW]
let%test _ = assert_equal_tokens "<<-" [LT; ASSIGN]

let long_comment = "(*" ^ (String.make 5000 ' ') ^ "*)"

let%test _ = assert_equal_token long_comment EOF

let%test _ = assert_equal_token "%" (ERR "%")
let%test _ = assert_equal_token "\\" (ERR "\\")
let%test _ = assert_equal_token "\n" EOF
let%test _ = assert_equal_tokens "123in" [INT_CONST "123"; IN]
let%test _ = assert_equal_token "in123" (OBJECTID "in123")

let long_str_a = String.make 1024 'a'
let%test _ = assert_equal_token (stringify long_str_a) (STR_CONST long_str_a)

let str_a_too_long = String.make 1025 'a'
let%test _ = assert_equal_token (stringify str_a_too_long) (ERR "String constant too long")

let long_esc_str = String.make 2048 '\\'
let long_esc_str_res = String.make 1024 '\\'
let%test _ = assert_equal_token (stringify long_esc_str) (STR_CONST long_esc_str_res)

let long_esc_str_too_long = String.make 2050 '\\'
let%test _ = assert_equal_token (stringify long_esc_str_too_long) (ERR "String constant too long")
