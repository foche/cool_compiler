(* astprint.ml *)

open Util
open Ast

let error = "\027[31mError:\027[0m"

let indent n =
  for _ = 1 to n do
    print_char ' '
  done

let dump_string n s = indent n ; print_endline s

let dump_string_escaped n handle =
  Strtbl.find Tables.str_const_tbl handle
  |> Printf.sprintf "%S" |> dump_string n

let print_id n handle = Strtbl.find Tables.id_tbl handle |> dump_string n

let print_typ n handle = Strtbl.find Tables.type_tbl handle |> dump_string n

let print_int_const n handle =
  Strtbl.find Tables.int_const_tbl handle |> dump_string n

let print_header n line_number name =
  Printf.sprintf "\027[35m#%d\027[0m" line_number |> dump_string n ;
  dump_string n name

let print_type n typ_opt =
  indent n ;
  print_string ": " ;
  match typ_opt with
  | None -> print_endline "_no_type"
  | Some handle -> print_typ 0 handle

let get_arith_name op =
  match op with
  | Add -> "_plus"
  | Sub -> "_sub"
  | Mul -> "_mul"
  | Div -> "_divide"

let get_comp_name op = match op with Lt -> "_lt" | Le -> "_leq"

let rec print_case n ((id, typ_opt, exp), line_number) =
  print_header n line_number "_branch" ;
  print_id (n + 2) id ;
  print_typ (n + 2) typ_opt ;
  print_expr (n + 2) exp

and print_e n line_number exp =
  let print_header_with_name = print_header n line_number in
  let print_e_list m = List.iter (print_expr m) in
  match exp with
  | Assign (id, exp') ->
      print_header_with_name "_assign" ;
      print_id (n + 2) id ;
      print_expr (n + 2) exp'
  | DynDispatch dyn ->
      print_header_with_name "_dispatch" ;
      print_expr (n + 2) dyn.dyn_recv ;
      print_id (n + 2) dyn.dyn_method ;
      dump_string (n + 2) "(" ;
      print_e_list (n + 2) dyn.dyn_args ;
      dump_string (n + 2) ")"
  | StaticDispatch stat ->
      print_header_with_name "_static_dispatch" ;
      print_expr (n + 2) stat.stat_recv ;
      print_typ (n + 2) stat.stat_type ;
      print_id (n + 2) stat.stat_method ;
      dump_string (n + 2) "(" ;
      print_e_list (n + 2) stat.stat_args ;
      dump_string (n + 2) ")"
  | Cond (e1, e2, e3) ->
      print_header_with_name "_cond" ;
      print_e_list (n + 2) [e1; e2; e3]
  | Loop (e1, e2) ->
      print_header_with_name "_loop" ;
      print_e_list (n + 2) [e1; e2]
  | Block exps ->
      print_header_with_name "_block" ;
      print_e_list (n + 2) exps
  | Let let_stmt ->
      print_header_with_name "_let" ;
      print_id (n + 2) let_stmt.let_var ;
      print_typ (n + 2) let_stmt.let_var_type ;
      print_expr (n + 2) let_stmt.let_init ;
      print_expr (n + 2) let_stmt.let_body
  | Case (exp', cases) ->
      print_header_with_name "_typcase" ;
      print_expr (n + 2) exp' ;
      List.iter (print_case (n + 2)) cases
  | New typ_opt ->
      print_header_with_name "_new" ;
      print_typ (n + 2) typ_opt
  | IsVoid exp' ->
      print_header_with_name "_isvoid" ;
      print_expr (n + 2) exp'
  | Arith (op, e1, e2) ->
      get_arith_name op |> print_header_with_name ;
      print_e_list (n + 2) [e1; e2]
  | Neg exp' ->
      print_header_with_name "_neg" ;
      print_expr (n + 2) exp'
  | Comp (op, e1, e2) ->
      get_comp_name op |> print_header_with_name ;
      print_e_list (n + 2) [e1; e2]
  | Eq (e1, e2) ->
      print_header_with_name "_eq" ;
      print_e_list (n + 2) [e1; e2]
  | Not exp' ->
      print_header_with_name "_comp" ;
      print_expr (n + 2) exp'
  | Variable handle ->
      print_header_with_name "_object" ;
      print_id (n + 2) handle
  | IntConst handle ->
      print_header_with_name "_int" ;
      print_int_const (n + 2) handle
  | StrConst handle ->
      print_header_with_name "_string" ;
      dump_string_escaped (n + 2) handle
  | BoolConst x ->
      print_header_with_name "_bool" ;
      dump_string (n + 2) (if x then "1" else "0")
  | NoExpr -> print_header_with_name "_no_expr"

and print_expr n (exp, line_number) =
  print_e n line_number exp.typ_expr ;
  print_type n exp.typ_type

let print_formal n ((arg, typ_opt), line_number) =
  print_header n line_number "_formal" ;
  print_id (n + 2) arg ;
  print_typ (n + 2) typ_opt

let print_method n line_number mthd =
  print_header n line_number "_method" ;
  print_id (n + 2) mthd.method_id ;
  List.iter (print_formal (n + 2)) mthd.method_args ;
  print_typ (n + 2) mthd.method_ret_type ;
  print_expr (n + 2) mthd.method_body

let print_field n line_number id typ_opt exp =
  print_header n line_number "_attr" ;
  print_id (n + 2) id ;
  print_typ (n + 2) typ_opt ;
  print_expr (n + 2) exp

let print_feature n (feat, line_number) =
  match feat with
  | Method mthd -> print_method n line_number mthd
  | Field (id, t, e) -> print_field n line_number id t e

let print_class n (cl, line_number) =
  print_header n line_number "_class" ;
  print_typ (n + 2) cl.class_type ;
  print_typ (n + 2) cl.class_parent ;
  dump_string_escaped (n + 2) cl.class_filename ;
  dump_string (n + 2) "(" ;
  List.iter (print_feature (n + 2)) cl.class_features ;
  dump_string (n + 2) ")"

let print_ast (classes, line_number) =
  print_header 0 line_number "_program" ;
  List.iter (print_class 2) classes

let print_syntax_error _ =
  prerr_endline "Compilation halted due to lex and parse errors"

let print_eof_error filename =
  Printf.eprintf "%s %S, line 0: syntax error at or near EOF\n" error filename

let err_unclosed tok pos_start pos_end =
  Printf.eprintf "File %S, line %d, characters %d-%d:\n%s Unclosed %S\n"
    pos_start.Lexing.pos_fname pos_start.Lexing.pos_lnum
    (pos_start.Lexing.pos_cnum - pos_start.Lexing.pos_bol + 1)
    (pos_end.Lexing.pos_cnum - pos_end.Lexing.pos_bol + 1)
    error tok

let err_expected msg pos_start pos_end =
  Printf.eprintf "File %S, line %d, characters %d-%d:\n%s Expected %s\n"
    pos_start.Lexing.pos_fname pos_start.Lexing.pos_lnum
    (pos_start.Lexing.pos_cnum - pos_start.Lexing.pos_bol + 1)
    (pos_end.Lexing.pos_cnum - pos_end.Lexing.pos_bol + 1)
    error msg

let err_syntax pos_start pos_end =
  Printf.eprintf "File %S, line %d, characters %d-%d:\n%s Syntax error\n"
    pos_start.Lexing.pos_fname pos_start.Lexing.pos_lnum
    (pos_start.Lexing.pos_cnum - pos_start.Lexing.pos_bol + 1)
    (pos_end.Lexing.pos_cnum - pos_end.Lexing.pos_bol + 1)
    error
