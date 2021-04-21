(* astprint.ml *)

open StdLabels
open Util
module Abssyn = Abstractsyntax

let error = "\027[31mError:\027[0m"

let indent n = String.make n ' ' |> print_string

let dump_string n s =
  indent n;
  print_endline s

let dump_string_escaped n handle =
  Strtbl.find Tables.str_const_tbl handle
  |> Printf.sprintf "%S" |> dump_string n

let print_id n handle = Strtbl.find Tables.id_tbl handle |> dump_string n

let print_type n handle = Strtbl.find Tables.type_tbl handle |> dump_string n

let print_int_const n handle =
  Strtbl.find Tables.int_const_tbl handle |> dump_string n

let print_header n (loc, _) name =
  Printf.sprintf "#%d" loc.Lexing.pos_lnum |> dump_string n;
  dump_string n name

let print_typee n typ_opt =
  indent n;
  print_string ": ";
  match typ_opt with
  | None -> print_endline "_no_type"
  | Some handle -> print_type 0 handle

let get_arith_name op =
  match op with
  | Abssyn.Plus -> "_plus"
  | Abssyn.Minus -> "_sub"
  | Abssyn.Mult -> "_mul"
  | Abssyn.Div -> "_divide"

let get_comp_name op = match op with Abssyn.Lt -> "_lt" | Abssyn.Le -> "_leq"

let rec print_branch n (branch : Abssyn.branch_node) =
  print_header n branch.loc "_branch";
  let (var, typ), expr = branch.elem in
  print_id (n + 2) var;
  print_type (n + 2) typ;
  print_expr (n + 2) expr

and print_e n line_number (exp : Abssyn.expr) =
  let print_header_with_name = print_header n line_number in
  let print_e_list m = List.iter ~f:(print_expr m) in
  match exp with
  | Assign (id, exp') ->
      print_header_with_name "_assign";
      print_id (n + 2) id;
      print_expr (n + 2) exp'
  | DynamicDispatch dyn ->
      print_header_with_name "_dispatch";
      print_expr (n + 2) dyn.dyn_recv;
      print_id (n + 2) dyn.dyn_method_id;
      dump_string (n + 2) "(";
      print_e_list (n + 2) dyn.dyn_args;
      dump_string (n + 2) ")"
  | StaticDispatch stat ->
      print_header_with_name "_static_dispatch";
      print_expr (n + 2) stat.stat_recv;
      print_type (n + 2) stat.stat_target_typ;
      print_id (n + 2) stat.stat_method_id;
      dump_string (n + 2) "(";
      print_e_list (n + 2) stat.stat_args;
      dump_string (n + 2) ")"
  | Cond cond ->
      print_header_with_name "_cond";
      print_e_list (n + 2) [ cond.cond_pred; cond.cond_true; cond.cond_false ]
  | Loop loop ->
      print_header_with_name "_loop";
      print_e_list (n + 2) [ loop.loop_pred; loop.loop_body ]
  | Block exps ->
      print_header_with_name "_block";
      print_e_list (n + 2) exps
  | Let let_stmt ->
      print_header_with_name "_let";
      let var, typ = let_stmt.let_var in
      print_id (n + 2) var;
      print_type (n + 2) typ;
      print_expr (n + 2) let_stmt.let_init;
      print_expr (n + 2) let_stmt.let_body
  | Case case ->
      print_header_with_name "_typcase";
      print_expr (n + 2) case.case_expr;
      List.iter ~f:(print_branch (n + 2)) case.case_branches
  | New typ ->
      print_header_with_name "_new";
      print_type (n + 2) typ
  | IsVoid exp' ->
      print_header_with_name "_isvoid";
      print_expr (n + 2) exp'
  | Arith arith ->
      get_arith_name arith.arith_op |> print_header_with_name;
      print_e_list (n + 2) [ arith.arith_e1; arith.arith_e2 ]
  | Neg exp' ->
      print_header_with_name "_neg";
      print_expr (n + 2) exp'
  | Comp comp ->
      get_comp_name comp.comp_op |> print_header_with_name;
      print_e_list (n + 2) [ comp.comp_e1; comp.comp_e2 ]
  | Eq (e1, e2) ->
      print_header_with_name "_eq";
      print_e_list (n + 2) [ e1; e2 ]
  | Not exp' ->
      print_header_with_name "_comp";
      print_expr (n + 2) exp'
  | Variable handle ->
      print_header_with_name "_object";
      print_id (n + 2) handle
  | IntConst handle ->
      print_header_with_name "_int";
      print_int_const (n + 2) handle
  | StrConst handle ->
      print_header_with_name "_string";
      dump_string_escaped (n + 2) handle
  | BoolConst x ->
      print_header_with_name "_bool";
      dump_string (n + 2) (if x then "1" else "0")
  | NoExpr -> print_header_with_name "_no_expr"

and print_expr n expr =
  print_e n expr.Abssyn.expr_loc expr.expr_expr;
  print_typee n expr.expr_typ

let print_formal n (formal : Abssyn.formal) =
  print_header n formal.loc "_formal";
  fst formal.elem |> print_id (n + 2);
  snd formal.elem |> print_type (n + 2)

let print_method n line_number method_def =
  print_header n line_number "_method";
  print_id (n + 2) method_def.Abssyn.method_id;
  List.iter ~f:(print_formal (n + 2)) method_def.method_formals;
  print_type (n + 2) method_def.method_ret_typ;
  print_expr (n + 2) method_def.method_body

let print_field n line_number var exp =
  print_header n line_number "_attr";
  fst var |> print_id (n + 2);
  snd var |> print_type (n + 2);
  print_expr (n + 2) exp

let print_feature n feature =
  match feature.Abssyn.elem with
  | Abssyn.Method method_def -> print_method n feature.loc method_def
  | Abssyn.Field (var, exp) -> print_field n feature.loc var exp

let print_class n (cl : Abssyn.class_node) =
  print_header n cl.loc "_class";
  print_type (n + 2) cl.elem.cl_typ;
  print_type (n + 2) cl.elem.cl_parent;
  Printf.sprintf "%S" (fst cl.loc).pos_fname |> dump_string (n + 2);
  dump_string (n + 2) "(";
  List.iter ~f:(print_feature (n + 2)) cl.elem.cl_features;
  dump_string (n + 2) ")"

let print_ast (program : Abssyn.program) =
  print_header 0 program.loc "_program";
  List.iter ~f:(print_class 2) program.elem

let print_syntax_error _ =
  prerr_endline "Compilation halted due to lex and parse errors"

let print_eof_error =
  Printf.eprintf "%s %S, line 0: syntax error at or near EOF\n" error
