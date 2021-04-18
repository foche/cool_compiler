(* optutil.ml *)

open StdLabels

let map2 ~f x y =
  Option.map (fun x' -> Option.map (fun y' -> f x' y') y) x |> Option.join

let singleton x_opt = Option.map (fun x -> [ x ]) x_opt

let merge x xs = map2 ~f:(fun x' xs' -> x' :: xs') x xs

let flatten_opt_list opt_list =
  List.fold_left ~f:(fun acc opt -> merge opt acc) ~init:(Some []) opt_list
