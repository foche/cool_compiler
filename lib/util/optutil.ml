(* optutil.ml *)

open StdLabels

let fold2 ~none ~some x y =
  match (x, y) with
  | Some x', Some y' -> some x' y'
  | Some _, None | None, Some _ | None, None -> none

let map2 ~f x y = fold2 ~none:None ~some:(fun x' y' -> Some (f x' y')) x y

let singleton x_opt = Option.map (fun x -> [ x ]) x_opt

let merge x xs = map2 ~f:List.cons x xs

let flatten_opt_list opt_list =
  List.fold_left ~f:(fun acc opt -> merge opt acc) ~init:(Some []) opt_list
