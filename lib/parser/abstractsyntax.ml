(* abstractsyntax.ml *)

open Util

type loc = Lexing.position * Lexing.position

type 'a with_pos = { elem : 'a; loc : loc }

type arith_op = Plus | Minus | Mult | Div

type comp = Lt | Le

type var_decl = Tables.id_sym * Tables.typ_sym

type var_node = var_decl with_pos

type expr =
  | Assign of Tables.id_sym * expr_node
  | DynamicDispatch of dynamic_dispatch_expr
  | StaticDispatch of static_dispatch_expr
  | Cond of cond_expr
  | Loop of loop_expr
  | Block of expr_node list * expr_node
  | Let of let_expr
  | Case of case_expr
  | New of Tables.typ_sym
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
  expr_typ : Tables.typ_sym option;
  expr_loc : loc;
}

and cond_expr = {
  cond_pred : expr_node;
  cond_true : expr_node;
  cond_false : expr_node;
}

and loop_expr = { loop_pred : expr_node; loop_body : expr_node }

and let_expr = {
  let_var : var_node;
  let_init : expr_node;
  let_body : expr_node;
}

and case_expr = { case_expr : expr_node; case_branches : branch_node list }

and arith_expr = {
  arith_op : arith_op;
  arith_e1 : expr_node;
  arith_e2 : expr_node;
}

and comp_expr = { comp_op : comp; comp_e1 : expr_node; comp_e2 : expr_node }

and dynamic_dispatch_expr = {
  dyn_recv : expr_node;
  dyn_method_id : Tables.id_sym;
  dyn_args : expr_node list;
}

and static_dispatch_expr = {
  stat_recv : expr_node;
  stat_target_typ : Tables.typ_sym;
  stat_method_id : Tables.id_sym;
  stat_args : expr_node list;
  stat_label : Tables.id_sym option;
}

and branch = { branch_var : var_node; branch_body : expr_node }

and branch_node = branch with_pos

type formal = var_node

type method_def = {
  method_id : Tables.id_sym;
  method_formals : formal list;
  method_ret_typ : Tables.typ_sym;
  method_body : expr_node;
}

type field_def = { field_var : var_node; field_init : expr_node }

type feature = Method of method_def | Field of field_def

type class_def = {
  cl_typ : Tables.typ_sym;
  cl_parent : Tables.typ_sym;
  cl_features : feature with_pos list;
}

type class_node = class_def with_pos

type class_list = class_node list

type program = class_list with_pos
