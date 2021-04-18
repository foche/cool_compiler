(* ast.ml *)

open StdLabels
open Util
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

let create_single ~f ~x ~loc =
  Option.map (fun x' -> create_expr ~expr:(f x') loc) x

let create_double ~f ~x ~y ~loc =
  Optutil.map2 ~f:(fun x' y' -> create_expr ~expr:(f x' y') loc) x y

let no_expr ~loc =
  {
    Abssyn.expr_expr = Abssyn.NoExpr;
    expr_typ = None;
    expr_loc = loc;
  }

let replace_expr ~new_expr expr = { expr with Abssyn.expr_expr = new_expr }

let add_type ~typ expr = { expr with Abssyn.expr_typ = Some typ }

let translate_type ~(cl : Abssyn.class_node) typ =
  if typ = Tables.self_type then cl.elem.cl_typ else typ

let is_subtype graph ~cl ~sub_typ ~super_typ =
  match sub_typ = super_typ with
  | true -> true
  | false -> (
      match super_typ = Tables.self_type with
      | true -> false
      | false ->
          translate_type ~cl sub_typ
          |> Tree.is_ancestor graph ~ancestor:super_typ)
