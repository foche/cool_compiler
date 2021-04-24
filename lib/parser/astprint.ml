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

let print_type_opt n typ_opt =
  indent n;
  print_string ": ";
  match typ_opt with
  | None -> print_endline "_no_type"
  | Some handle -> print_type 0 handle

let get_arith_name = function
  | Abssyn.Plus -> "_plus"
  | Abssyn.Minus -> "_sub"
  | Abssyn.Mult -> "_mul"
  | Abssyn.Div -> "_divide"

let get_comp_name = function Abssyn.Lt -> "_lt" | Abssyn.Le -> "_leq"

let rec print_branch n { Abssyn.elem = { Abssyn.branch_var; branch_body }; loc }
    =
  print_header n loc "_branch";
  let var, typ = branch_var.Abssyn.elem in
  print_id (n + 2) var;
  print_type (n + 2) typ;
  print_expr (n + 2) branch_body

and print_e n line_number (expr : Abssyn.expr) =
  let print_header_with_name = print_header n line_number in
  let print_e_list = List.iter ~f:(print_expr (n + 2)) in
  match expr with
  | Assign (id, expr') ->
      print_header_with_name "_assign";
      print_id (n + 2) id;
      print_expr (n + 2) expr'
  | DynamicDispatch { dyn_recv; dyn_method_id; dyn_args } ->
      print_header_with_name "_dispatch";
      print_expr (n + 2) dyn_recv;
      print_id (n + 2) dyn_method_id;
      dump_string (n + 2) "(";
      print_e_list dyn_args;
      dump_string (n + 2) ")"
  | StaticDispatch { stat_recv; stat_target_typ; stat_method_id; stat_args; _ }
    ->
      print_header_with_name "_static_dispatch";
      print_expr (n + 2) stat_recv;
      print_type (n + 2) stat_target_typ;
      print_id (n + 2) stat_method_id;
      dump_string (n + 2) "(";
      print_e_list stat_args;
      dump_string (n + 2) ")"
  | Cond { cond_pred; cond_true; cond_false } ->
      print_header_with_name "_cond";
      print_e_list [ cond_pred; cond_true; cond_false ]
  | Loop { loop_pred; loop_body } ->
      print_header_with_name "_loop";
      print_e_list [ loop_pred; loop_body ]
  | Block (stmts, expr) ->
      print_header_with_name "_block";
      print_e_list stmts;
      print_expr (n + 2) expr
  | Let { let_var = { elem = var, typ; _ }; let_init; let_body } ->
      print_header_with_name "_let";
      print_id (n + 2) var;
      print_type (n + 2) typ;
      print_expr (n + 2) let_init;
      print_expr (n + 2) let_body
  | Case { case_expr; case_branches } ->
      print_header_with_name "_typcase";
      print_expr (n + 2) case_expr;
      List.iter ~f:(print_branch (n + 2)) case_branches
  | New typ ->
      print_header_with_name "_new";
      print_type (n + 2) typ
  | IsVoid expr' ->
      print_header_with_name "_isvoid";
      print_expr (n + 2) expr'
  | Arith { arith_op; arith_e1; arith_e2 } ->
      get_arith_name arith_op |> print_header_with_name;
      print_e_list [ arith_e1; arith_e2 ]
  | Neg expr' ->
      print_header_with_name "_neg";
      print_expr (n + 2) expr'
  | Comp { comp_op; comp_e1; comp_e2 } ->
      get_comp_name comp_op |> print_header_with_name;
      print_e_list [ comp_e1; comp_e2 ]
  | Eq (e1, e2) ->
      print_header_with_name "_eq";
      print_e_list [ e1; e2 ]
  | Not expr' ->
      print_header_with_name "_comp";
      print_expr (n + 2) expr'
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

and print_expr n { Abssyn.expr_expr; expr_typ; expr_loc } =
  print_e n expr_loc expr_expr;
  print_type_opt n expr_typ

let print_formal n { Abssyn.elem = id, typ; loc } =
  print_header n loc "_formal";
  print_id (n + 2) id;
  print_type (n + 2) typ

let print_method n line_number
    { Abssyn.method_id; method_formals; method_ret_typ; method_body } =
  print_header n line_number "_method";
  print_id (n + 2) method_id;
  List.iter ~f:(print_formal (n + 2)) method_formals;
  print_type (n + 2) method_ret_typ;
  print_expr (n + 2) method_body

let print_field n line_number { Abssyn.field_var; field_init } =
  print_header n line_number "_attr";
  let { Abssyn.elem = id, typ; _ } = field_var in
  print_id (n + 2) id;
  print_type (n + 2) typ;
  print_expr (n + 2) field_init

let print_feature n { Abssyn.elem; loc } =
  match elem with
  | Abssyn.Method method_def -> print_method n loc method_def
  | Abssyn.Field field_def -> print_field n loc field_def

let print_class n
    {
      Abssyn.elem = { Abssyn.cl_typ; cl_parent; cl_features };
      loc = ({ pos_fname; _ }, _) as loc;
    } =
  print_header n loc "_class";
  print_type (n + 2) cl_typ;
  print_type (n + 2) cl_parent;
  Printf.sprintf "%S" pos_fname |> dump_string (n + 2);
  dump_string (n + 2) "(";
  List.iter ~f:(print_feature (n + 2)) cl_features;
  dump_string (n + 2) ")"

let print_ast { Abssyn.elem; loc } =
  print_header 0 loc "_program";
  List.iter ~f:(print_class 2) elem

let print_syntax_error _ =
  prerr_endline "Compilation halted due to lex and parse errors"

let print_eof_error =
  Printf.eprintf "%s %S, line 0: syntax error at or near EOF\n" error
