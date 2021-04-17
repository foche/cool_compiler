(* optutil.ml *)

open StdLabels

let get ?default opt =
  match (opt, default) with
  | Some x, _ -> x
  | None, Some x -> x
  | None, None -> invalid_arg "Get option with None and no default"

let map ~f opt = match opt with Some x -> f x | None -> None

let map2 ~f opt1 opt2 =
  match (opt1, opt2) with Some x, Some y -> f x y | _ -> None

let singleton x_opt = map ~f:(fun x -> Some [x]) x_opt

let merge x_opt xs_opt = map2 ~f:(fun x xs -> Some (x :: xs)) x_opt xs_opt

let is_some opt = match opt with Some _ -> true | None -> false

let is_none opt = is_some opt |> not

let flatten_opt_list opt_list =
  List.fold_left ~f:(fun acc opt -> merge opt acc) ~init:(Some []) opt_list
