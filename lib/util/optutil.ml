(* optutil.ml *)

open StdLabels

let map2 ~f x y =
  match (x, y) with Some x', Some y' -> Some (f x' y') | _ -> None

let singleton x_opt = Option.map (fun x -> [ x ]) x_opt

let merge x xs = map2 ~f:List.cons x xs

let flatten_opt_list opt_list =
  List.fold_left ~f:(fun acc opt -> merge opt acc) ~init:(Some []) opt_list
