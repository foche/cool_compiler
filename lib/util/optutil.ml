(* optutil.ml *)

open! StdLabels

let fold2 ~none ~some x y =
  match (x, y) with
  | Some x', Some y' -> some x' y'
  | Some _, None | None, Some _ | None, None -> none

let bind2 ~f x y = fold2 ~none:None ~some:f x y

let map2 ~f x y = bind2 ~f:(fun x' y' -> Some (f x' y')) x y

let singleton x = Option.map (fun x' -> [ x' ]) x

let merge x xs = map2 ~f:List.cons x xs

let flatten_opt_list xs =
  List.fold_left ~f:(fun acc x -> merge x acc) ~init:(Some []) xs
