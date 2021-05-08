open! StdLabels
module Tbls = Util.Tables
module Abssyn = Abstractsyntax

type raw_pos = Location.raw_pos

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

val replace_features :
  cl:Abssyn.class_node -> Abssyn.feature_node List.t -> Abssyn.class_node

val iter_features :
  method_f:(loc:Location.t -> Abssyn.method_def -> Unit.t) ->
  field_f:(loc:Location.t -> Abssyn.field_def -> Unit.t) ->
  Abssyn.class_node ->
  Unit.t

val for_all_features :
  method_f:(loc:Location.t -> Abssyn.method_def -> Bool.t) ->
  field_f:(loc:Location.t -> Abssyn.field_def -> Bool.t) ->
  Abssyn.class_node ->
  Bool.t

val rev_map_features :
  method_f:(loc:Location.t -> Abssyn.method_def -> 'a) ->
  field_f:(loc:Location.t -> Abssyn.field_def -> 'a) ->
  Abssyn.class_node ->
  'a List.t
