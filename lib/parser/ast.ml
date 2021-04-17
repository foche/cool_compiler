(* ast.ml *)

open StdLabels
open Util
module Abssyn = Abstractsyntax

let create_expr ?typ ~expr startpos endpos =
  {Abssyn.expr= expr; typ; startpos; endpos}

let create_let ~bindings ~body =
  let x, typ, init, startpos, endpos = List.hd bindings in
  let last_binding =
    create_expr
      ~expr:
        (Abssyn.Let {var= (x, typ); init; body})
      startpos endpos
  in
  let f body' (x', typ', init', startpos', endpos') =
    create_expr
      ~expr:
        (Abssyn.Let {var= (x', typ'); init= init'; body= body'})
      startpos' endpos'
  in
  List.tl bindings |> List.fold_left ~f ~init:last_binding

let create_single ~f ~x ~startpos ~endpos =
  Optutil.map ~f:(fun x' -> Some (create_expr ~expr:(f x') startpos endpos)) x

let create_double ~f ~x ~y ~startpos ~endpos =
  Optutil.map2 ~f:(fun x' y' -> Some (create_expr ~expr:(f x' y') startpos endpos)) x y

let no_expr ~startpos ~endpos = {Abssyn.expr= Abssyn.NoExpr; typ= None; startpos; endpos}

let add_type node typ =
  {node with Abssyn.typ= typ}

let translate_type cl typ =
  if typ = Tables.self_type then cl.Abssyn.typ else typ

let is_subtype graph cl typ1 typ2 =
  match typ1 = typ2 with
  | true -> true
  | false -> (
    match typ2 = Tables.self_type with
    | true -> false
    | false -> translate_type cl typ1 |> Tree.is_ancestor graph ~ancestor:typ2 )
