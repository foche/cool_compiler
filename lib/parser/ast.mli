open! StdLabels
module Tbls = Util.Tables
module Abssyn = Abstractsyntax

type raw_pos = Lexing.position * Lexing.position

val create_node : loc:raw_pos -> 'a -> 'a Abssyn.with_pos

val create_expr :
  ?typ:Tbls.typ_sym -> loc:raw_pos -> Abssyn.expr -> Abssyn.expr_node

val create_let :
  bindings:(Abssyn.var_node * Abssyn.expr_node * raw_pos) List.t ->
  body:Abssyn.expr_node ->
  Abssyn.expr_node

val no_expr : loc:raw_pos -> Abssyn.expr_node

val replace_expr :
  typ:Tbls.typ_sym -> expr:Abssyn.expr_node -> Abssyn.expr -> Abssyn.expr_node

val add_type : typ:Tbls.typ_sym -> Abssyn.expr_node -> Abssyn.expr_node

val create_var_node :
  id:String.t -> typ:String.t -> loc:raw_pos -> Abssyn.var_node

val self_var_expr : loc:raw_pos -> Abssyn.expr_node
