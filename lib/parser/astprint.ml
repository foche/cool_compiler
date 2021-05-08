(* astprint.ml *)

open! StdLabels
module Tbls = Util.Tables
module Abssyn = Abstractsyntax

let rec print_list printer = function
  | [] -> ()
  | [ x ] -> printer x
  | x :: y :: xs ->
      printer x;
      Format.print_cut ();
      print_list printer (y :: xs)

let print_id = Format.printf "%a" Tbls.print_id

let print_type = Format.printf "%a" Tbls.print_type

let print_str_const = Format.printf "%a" Tbls.print_str

let print_int_const = Format.printf "%a" Tbls.print_int

let print_header loc =
  Location.start_line_num loc |> Format.printf "#%d@,@[<v 2>%s"

let print_type_opt typ_opt =
  Format.printf ": ";
  match typ_opt with
  | None -> Format.printf "_no_type"
  | Some handle -> print_type handle

let get_arith_name = function
  | Abssyn.Plus -> "_plus"
  | Abssyn.Minus -> "_sub"
  | Abssyn.Mult -> "_mul"
  | Abssyn.Div -> "_divide"

let get_comp_name = function Abssyn.Lt -> "_lt" | Abssyn.Le -> "_leq"

let rec print_branch { Abssyn.elem = { Abssyn.branch_var; branch_body }; loc } =
  print_header loc "_branch";
  Format.print_cut ();
  let var, typ = branch_var.Abssyn.elem in
  print_id var;
  Format.print_cut ();
  print_type typ;
  Format.print_cut ();
  print_expr branch_body;
  Format.printf "@]"

and print_e loc expr =
  let print_header_with_name = print_header loc in
  let print_e_list = print_list print_expr in
  (match expr with
  | Abssyn.Assign (id, sub_expr) ->
      print_header_with_name "_assign";
      Format.print_cut ();
      print_id id;
      Format.print_cut ();
      print_expr sub_expr
  | Abssyn.DynamicDispatch { Abssyn.dyn_recv; dyn_method_id; dyn_args } ->
      print_header_with_name "_dispatch";
      Format.print_cut ();
      print_expr dyn_recv;
      Format.print_cut ();
      print_id dyn_method_id;
      Format.printf "@,(@,";
      List.iter
        ~f:(fun arg ->
          print_expr arg;
          Format.print_cut ())
        dyn_args;
      Format.printf ")"
  | Abssyn.StaticDispatch
      { Abssyn.stat_recv; stat_target_typ; stat_method_id; stat_args; _ } ->
      print_header_with_name "_static_dispatch";
      Format.print_cut ();
      print_expr stat_recv;
      Format.print_cut ();
      print_type stat_target_typ;
      Format.print_cut ();
      print_id stat_method_id;
      Format.printf "@,(@,";
      List.iter
        ~f:(fun arg ->
          print_expr arg;
          Format.print_cut ())
        stat_args;
      Format.printf ")"
  | Abssyn.Cond { Abssyn.cond_pred; cond_true; cond_false } ->
      print_header_with_name "_cond";
      Format.print_cut ();
      print_e_list [ cond_pred; cond_true; cond_false ]
  | Abssyn.Loop { Abssyn.loop_pred; loop_body } ->
      print_header_with_name "_loop";
      Format.print_cut ();
      print_e_list [ loop_pred; loop_body ]
  | Abssyn.Block (stmts, sub_expr) ->
      print_header_with_name "_block";
      Format.print_cut ();
      List.iter
        ~f:(fun stmt ->
          print_expr stmt;
          Format.print_cut ())
        stmts;
      print_expr sub_expr
  | Abssyn.Let
      { Abssyn.let_var = { Abssyn.elem = var, typ; _ }; let_init; let_body } ->
      print_header_with_name "_let";
      Format.print_cut ();
      print_id var;
      Format.print_cut ();
      print_type typ;
      Format.print_cut ();
      print_expr let_init;
      Format.print_cut ();
      print_expr let_body
  | Abssyn.Case { Abssyn.case_expr; case_branches } ->
      print_header_with_name "_typcase";
      Format.print_cut ();
      print_expr case_expr;
      Format.print_cut ();
      print_list print_branch case_branches
  | Abssyn.New typ ->
      print_header_with_name "_new";
      Format.print_cut ();
      print_type typ
  | Abssyn.IsVoid sub_expr ->
      print_header_with_name "_isvoid";
      Format.print_cut ();
      print_expr sub_expr
  | Abssyn.Arith { Abssyn.arith_op; arith_e1; arith_e2 } ->
      get_arith_name arith_op |> print_header_with_name;
      Format.print_cut ();
      print_e_list [ arith_e1; arith_e2 ]
  | Abssyn.Neg sub_expr ->
      print_header_with_name "_neg";
      Format.print_cut ();
      print_expr sub_expr
  | Abssyn.Comp { Abssyn.comp_op; comp_e1; comp_e2 } ->
      get_comp_name comp_op |> print_header_with_name;
      Format.print_cut ();
      print_e_list [ comp_e1; comp_e2 ]
  | Abssyn.Eq (e1, e2) ->
      print_header_with_name "_eq";
      Format.print_cut ();
      print_e_list [ e1; e2 ]
  | Abssyn.Not sub_expr ->
      print_header_with_name "_comp";
      Format.print_cut ();
      print_expr sub_expr
  | Abssyn.Variable handle ->
      print_header_with_name "_object";
      Format.print_cut ();
      print_id handle
  | Abssyn.IntConst handle ->
      print_header_with_name "_int";
      Format.print_cut ();
      print_int_const handle
  | Abssyn.StrConst handle ->
      print_header_with_name "_string";
      Format.print_cut ();
      print_str_const handle
  | Abssyn.BoolConst x ->
      print_header_with_name "_bool";
      Format.print_cut ();
      Format.printf (if x then "1" else "0")
  | Abssyn.NoExpr -> print_header_with_name "_no_expr");
  Format.printf "@]"

and print_expr { Abssyn.expr_expr; expr_typ; expr_loc } =
  print_e expr_loc expr_expr;
  Format.print_cut ();
  print_type_opt expr_typ

let print_formal { Abssyn.elem = id, typ; loc } =
  print_header loc "_formal";
  Format.print_cut ();
  print_id id;
  Format.print_cut ();
  print_type typ;
  Format.printf "@]@,"

let print_method ~loc
    { Abssyn.method_id; method_formals; method_ret_typ; method_body } =
  print_header loc "_method";
  Format.print_cut ();
  print_id method_id;
  Format.print_cut ();
  List.iter ~f:print_formal method_formals;
  print_type method_ret_typ;
  Format.print_cut ();
  print_expr method_body;
  Format.printf "@]@,"

let print_field ~loc
    { Abssyn.field_var = { Abssyn.elem = id, typ; _ }; field_init } =
  print_header loc "_attr";
  Format.print_cut ();
  print_id id;
  Format.print_cut ();
  print_type typ;
  Format.print_cut ();
  print_expr field_init;
  Format.printf "@]@,"

let print_class cl =
  let { Abssyn.elem = { Abssyn.cl_typ; cl_parent; _ }; loc } = cl in
  print_header loc "_class";
  Format.print_cut ();
  print_type cl_typ;
  Format.print_cut ();
  print_type cl_parent;
  Location.filename loc |> Format.printf "@,%S@,(@,";
  Ast.iter_features ~method_f:print_method ~field_f:print_field cl;
  Format.printf ")@]"

let print_ast { Abssyn.elem; loc } =
  Format.printf "@[<v>";
  print_header loc "_program";
  Format.print_cut ();
  print_list print_class elem;
  Format.printf "@]@."

let print_syntax_error _ =
  Format.eprintf "@[<v>Compilation halted due to lex and parse errors.@]@."

let print_eof_error =
  Format.eprintf
    "@[<v>File %S, line 0:@,\
     \027[31mError:\027[0m Syntax error at or near EOF.@]@."
