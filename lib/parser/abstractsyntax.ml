(* abstractsyntax.ml *)

open Util

type 'a with_pos = {elem: 'a; startpos: Lexing.position; endpos: Lexing.position}

type arith_op = Plus | Minus | Mult | Div

type comp = Lt | Le

type var_decl = Tables.id_sym * Tables.type_sym

type expr =
  | Assign of Tables.id_sym * expr_node
  | DynamicDispatch of {recv: expr_node; method_id: Tables.id_sym; args: expr_node list}
  | StaticDispatch of static_dispatch
  | Cond of {pred: expr_node; true_branch: expr_node; false_branch: expr_node}
  | Loop of {pred: expr_node; body: expr_node}
  | Block of expr_node list
  | Let of {var: var_decl; init: expr_node; body: expr_node}
  | Case of {expr: expr_node; branches: branch_node list}
  | New of Tables.type_sym
  | IsVoid of expr_node
  | Arith of {op: arith_op; e1: expr_node; e2: expr_node}
  | Neg of expr_node
  | Comp of {comp: comp; e1: expr_node; e2: expr_node}
  | Eq of expr_node * expr_node
  | Not of expr_node
  | Variable of Tables.id_sym
  | IntConst of Tables.int_sym
  | StrConst of Tables.str_sym
  | BoolConst of bool
  | NoExpr

and expr_node =
  { expr: expr
  ; typ: Tables.type_sym option
  ; startpos: Lexing.position
  ; endpos: Lexing.position }

and static_dispatch =
  { recv: expr_node
  ; target: Tables.type_sym
  ; method_id: Tables.id_sym
  ; args: expr_node list
  ; label: Tables.id_sym option }

and let_statement =
  { var: var_decl
  ; init: expr_node
  ; body: expr_node }

and branch_node = (var_decl * expr_node) with_pos

type formal = var_decl with_pos

type mthd =
  { method_id: Tables.id_sym
  ; formals: formal list
  ; ret_type: Tables.type_sym
  ; body: expr_node }

type feature = Method of mthd | Field of var_decl * expr_node

type class_def =
  { typ: Tables.type_sym
  ; parent: Tables.type_sym
  ; features: feature with_pos list }

type class_node = class_def with_pos

type class_list = class_node list

type program = class_list with_pos
