(* ast.ml *)

open StdLabels
module Abssyn = Abstractsyntax

let create_expr ?typ ~expr expr_loc =
  { Abssyn.expr_expr = expr; expr_typ = typ; expr_loc }

let create_let ~bindings ~body =
  let x, typ, let_init, loc = List.hd bindings in
  let last_binding =
    create_expr
      ~expr:(Abssyn.Let { let_var = (x, typ); let_init; let_body = body })
      loc
  in
  let f body' (x', typ', init', loc') =
    create_expr
      ~expr:
        (Abssyn.Let { let_var = (x', typ'); let_init = init'; let_body = body' })
      loc'
  in
  List.tl bindings |> List.fold_left ~f ~init:last_binding

let no_expr ~loc = create_expr ~expr:Abssyn.NoExpr loc

let replace_expr ~new_expr expr = { expr with Abssyn.expr_expr = new_expr }

let add_type ~typ expr = { expr with Abssyn.expr_typ = Some typ }
