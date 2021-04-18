(* abstractsyntax.ml *)

open Util

type 'a with_pos = {
  elem : 'a;
  loc : Lexing.position * Lexing.position;
}

type arith_op = Plus | Minus | Mult | Div

type comp = Lt | Le

type var_decl = Tables.id_sym * Tables.type_sym

type expr =
  | Assign of Tables.id_sym * expr_node
  | DynamicDispatch of dynamic_dispatch_expr
  | StaticDispatch of static_dispatch_expr
  | Cond of cond_expr
  | Loop of loop_expr
  | Block of expr_node list
  | Let of let_expr
  | Case of case_expr
  | New of Tables.type_sym
  | IsVoid of expr_node
  | Arith of arith_expr
  | Neg of expr_node
  | Comp of comp_expr
  | Eq of expr_node * expr_node
  | Not of expr_node
  | Variable of Tables.id_sym
  | IntConst of Tables.int_sym
  | StrConst of Tables.str_sym
  | BoolConst of bool
  | NoExpr

and expr_node = {
  expr_expr : expr;
  expr_typ : Tables.type_sym option;
  expr_loc : Lexing.position * Lexing.position;
}

and cond_expr = {
  cond_pred : expr_node;
  cond_true : expr_node;
  cond_false : expr_node;
}

and loop_expr = { loop_pred : expr_node; loop_body : expr_node }

and let_expr = { let_var : var_decl; let_init : expr_node; let_body : expr_node }

and case_expr = { case_expr : expr_node; case_branches : branch_node list }

and arith_expr = { arith_op : arith_op; arith_e1 : expr_node; arith_e2 : expr_node }

and comp_expr = { comp_op : comp; comp_e1 : expr_node; comp_e2 : expr_node }

and dynamic_dispatch_expr = {
  dyn_recv : expr_node;
  dyn_method_id : Tables.id_sym;
  dyn_args : expr_node list;
}

and static_dispatch_expr = {
  stat_recv : expr_node;
  stat_target_typ : Tables.type_sym;
  stat_method_id : Tables.id_sym;
  stat_args : expr_node list;
  stat_label : Tables.id_sym option;
}

and branch_node = (var_decl * expr_node) with_pos

type formal = var_decl with_pos

type method_def = {
  method_id : Tables.id_sym;
  method_formals : formal list;
  method_ret_typ : Tables.type_sym;
  method_body : expr_node;
}

type feature = Method of method_def | Field of var_decl * expr_node

type class_def = {
  cl_typ : Tables.type_sym;
  cl_parent : Tables.type_sym;
  cl_features : feature with_pos list;
}

type class_node = class_def with_pos

type class_list = class_node list

type program = class_list with_pos
