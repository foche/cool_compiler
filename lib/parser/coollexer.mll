(* coollexer.mll *)

{
(*
 * Generated by coollexer.mll
 *
 * Lexer for the Classroom Object-Oriented Language (COOL).
 *)

open! StdLabels
module Parse = Coolparser
module Print = Lexerprint

let lexer_debug = ref false

let max_str_len = 1024

let make_bool_const s x =
  if s.[0] = (if x then 't' else 'f') then Parse.BOOL_CONST x else Parse.TYPEID s

let process_word s =
  (* try to match with keywords *)
  match String.lowercase_ascii s with
  | "class" -> Parse.CLASS
  | "else" -> Parse.ELSE
  | "fi" -> Parse.FI
  | "if" -> Parse.IF
  | "inherits" -> Parse.INHERITS
  | "in" -> Parse.IN
  | "let" -> Parse.LET
  | "loop" -> Parse.LOOP
  | "pool" -> Parse.POOL
  | "then" -> Parse.THEN
  | "while" -> Parse.WHILE
  | "case" -> Parse.CASE
  | "esac" -> Parse.ESAC
  | "of" -> Parse.OF
  | "new" -> Parse.NEW
  | "isvoid" -> Parse.ISVOID
  | "not" -> Parse.NOT
  | "true" -> make_bool_const s true
  | "false" -> make_bool_const s false
  | _ -> if Char.lowercase_ascii s.[0] = s.[0] then Parse.OBJECTID s else Parse.TYPEID s

let print_filename filename =
  if !lexer_debug then Print.print_filename filename
}

(* rules *)

let space = [' ' '\r' '\t' '\011' (* vertical tab *) '\012' (* form feed *)]
let digit = ['0' - '9']
let alpha = ['A'-'Z' 'a'-'z']
let ch = alpha | digit | '_'

(* Parse the next token *)
rule next_token = parse
  | space+ | "--" [^ '\n']*
      { next_token lexbuf } (* skip comments and whitespace except new line *)
  | '\n'
      { Lexing.new_line lexbuf; next_token lexbuf }
  | alpha ch* as s
      { process_word s } (* identifiers and keywords *)
  | digit+ as n
      { Parse.INT_CONST n }
  | '"'
      { str_const (Buffer.create 16) 0 lexbuf } (* string constants *)
  | "(*"
      { skip_comment 0 lexbuf } (* nested multiline comments *)
  | "*)"
      { Parse.ERR "Unmatched *)" }
  | "=>"
      { Parse.DARROW }
  | "<-"
      { Parse.ASSIGN }
  | "<="
      { Parse.LE }
  | '<'
      { Parse.LT }
  | '='
      { Parse.EQ }
  | '+'
      { Parse.PLUS }
  | '-'
      { Parse.MINUS }
  | '*'
      { Parse.MULT }
  | '/'
      { Parse.DIV }
  | '~'
      { Parse.NEG }
  | '@'
      { Parse.AT }
  | '.'
      { Parse.DOT }
  | ','
      { Parse.COMMA }
  | ':'
      { Parse.COLON }
  | ';'
      { Parse.SEMI }
  | '{'
      { Parse.LBRACE }
  | '}'
      { Parse.RBRACE }
  | '('
      { Parse.LPAREN }
  | ')'
      { Parse.RPAREN }
  | eof
      { Parse.EOF }
  | _
      { Parse.ERR (Lexing.lexeme lexbuf) }

(* Skip over multiline comment *)
and skip_comment depth = parse
  | "(*"
      { skip_comment (depth + 1) lexbuf } (* nested comment *)
  | "*)"
      {
        if depth = 0 then
          next_token lexbuf (* matching closing comment *)
        else skip_comment (depth - 1) lexbuf
      }
  | '\n'
      { Lexing.new_line lexbuf; skip_comment depth lexbuf }
  (* match anything until we see one of the previous classes *)
  | [^ '(' '*' '\n']+
      { skip_comment depth lexbuf }
  | eof
      { Parse.ERR "EOF in comment" }
  | _
      { skip_comment depth lexbuf }

(* Parse string constant *)
and str_const strbuf n = parse
  | '"'
      {
        if n > max_str_len then
          Parse.ERR "String constant too long"
        else Parse.STR_CONST (Buffer.contents strbuf)
      }
  | "\\b"
      { Buffer.add_char strbuf '\b'; str_const strbuf (n + 1) lexbuf }
  | "\\n"
      { Buffer.add_char strbuf '\n'; str_const strbuf (n + 1) lexbuf }
  | "\\t"
      { Buffer.add_char strbuf '\t'; str_const strbuf (n + 1) lexbuf }
  | "\\f"
      { Buffer.add_char strbuf '\012'; str_const strbuf (n + 1) lexbuf }
  | "\\\""
      { Buffer.add_char strbuf '"'; str_const strbuf (n + 1) lexbuf }
  | "\\\\"
      { Buffer.add_char strbuf '\\'; str_const strbuf (n + 1) lexbuf }
  | "\\\n"
      {
        Buffer.add_char strbuf '\n';
        Lexing.new_line lexbuf;
        str_const strbuf (n + 1) lexbuf
      }
  | '\\'
      { str_const strbuf n lexbuf } (* ignore single back slashes *)
  | '\000'
      { (* consume characters until the end of the string on null char *)
        str_const strbuf (n + 1) lexbuf |> ignore;
        Parse.ERR "String contains null character."
      }
  | '\n'
      { Lexing.new_line lexbuf; Parse.ERR "Unterminated string constant" }
  | [^ '\\' '\n' '\000' '"']+
      {
        let s = Lexing.lexeme lexbuf in
        Buffer.add_string strbuf s;
        str_const strbuf (n + String.length s) lexbuf
      }
  | eof
      { Parse.ERR "EOF in string constant" }
  | _ as c
      { Buffer.add_char strbuf c; str_const strbuf (n + 1) lexbuf }

{
let get_token lexbuf =
  let tok = next_token lexbuf in
  if !lexer_debug then (
    let line_num = lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum in
    Print.print_token tok line_num);
  tok
}
