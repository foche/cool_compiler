(* ast.ml *)

open StdLabels
open Util
module Abssyn = Abstractsyntax

let create_expr ?typ ~expr expr_loc =
  { Abssyn.expr_expr = expr; expr_typ = typ; expr_loc }

let create_let ~bindings ~body =
  List.fold_left
    ~f:(fun let_body (let_var, let_init, loc) ->
      create_expr ~expr:(Abssyn.Let { let_var; let_init; let_body }) loc)
    ~init:body bindings

let no_expr ~loc = create_expr ~expr:Abssyn.NoExpr loc

let replace_expr ~expr ~new_expr ~typ =
  { expr with Abssyn.expr_expr = new_expr; expr_typ = Some typ }

let add_type ~typ expr = { expr with Abssyn.expr_typ = Some typ }

let create_var_decl ~id ~typ = (Tables.make_id id, Tables.make_type typ)

let self_var_expr ~loc = create_expr ~expr:(Abssyn.Variable Tables.self_var) loc
