(** parsertests.ml *)

open Parser
open Coolparser

let assert_equal_token s tok =
  let ic = Lexing.from_string s in
  tok = Coollexer.next_token ic && EOF = Coollexer.next_token ic

let assert_equal_tokens s toks =
  let ic = Lexing.from_string s in
  List.for_all (fun tok -> tok = Coollexer.next_token ic) toks &&
  EOF = Coollexer.next_token ic

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

let%test _ = assert_equal_token "(**)" EOF
let%test _ = assert_equal_token "(* -- *)" EOF
let%test _ = assert_equal_token "(* \000 *)" EOF
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
  assert_equal_tokens
    "(*\"(*\"*)\"*)\"(*\"*)\"*)(*\""
    [STR_CONST "(*"; ERR "Unmatched *)"; STR_CONST "*)(*"]

let%test _ = assert_equal_token "%" (ERR "%")
