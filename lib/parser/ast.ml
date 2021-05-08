(* ast.ml *)

open! StdLabels
module Tbls = Util.Tables
module Abssyn = Abstractsyntax

type raw_pos = Lexing.position * Lexing.position

let create_node ~loc elem = { Abssyn.elem; loc = Location.create loc }

let create_expr ?typ ~loc expr =
  { Abssyn.expr_expr = expr; expr_typ = typ; expr_loc = Location.create loc }

let create_let ~bindings ~body =
  List.fold_left
    ~f:(fun let_body (let_var, let_init, loc) ->
      create_expr ~loc (Abssyn.Let { Abssyn.let_var; let_init; let_body }))
    ~init:body bindings

let create_dyn ~dyn_recv ~dyn_method_id ~dyn_args ~loc =
  Abssyn.DynamicDispatch
    { Abssyn.dyn_recv; dyn_method_id; dyn_args; dyn_is_tail = false }
  |> create_expr ~loc

let no_expr ~loc = create_expr ~loc Abssyn.NoExpr

let replace_expr ~typ ~expr new_expr =
  { expr with Abssyn.expr_expr = new_expr; expr_typ = Some typ }

let add_type ~typ expr = { expr with Abssyn.expr_typ = Some typ }

let create_var_node ~id ~typ ~loc =
  (Tbls.make_id id, Tbls.make_type typ) |> create_node ~loc

let self_var_expr ~loc = create_expr ~loc (Abssyn.Variable Tbls.self_var)

let replace_features ~cl cl_features =
  { cl with Abssyn.elem = { cl.Abssyn.elem with Abssyn.cl_features } }

let match_feature ~method_f ~field_f { Abssyn.elem; loc } =
  match elem with
  | Abssyn.Method method_def -> method_f ~loc method_def
  | Abssyn.Field field_def -> field_f ~loc field_def

let iter_features ~method_f ~field_f cl =
  List.iter
    ~f:(match_feature ~method_f ~field_f)
    cl.Abssyn.elem.Abssyn.cl_features

let for_all_features ~method_f ~field_f cl =
  List.for_all
    ~f:(match_feature ~method_f ~field_f)
    cl.Abssyn.elem.Abssyn.cl_features

let rev_map_features ~method_f ~field_f cl =
  List.rev_map
    ~f:(match_feature ~method_f ~field_f)
    cl.Abssyn.elem.Abssyn.cl_features
