(* helpers.ml *)

let get_opt ?(default = None) opt =
  match opt, default with
  | Some x, _ -> x
  | None, Some x -> x
  | None, None -> raise Not_found

let map_opt ~f opt =
  match opt with
  | Some x -> Some (f x)
  | None -> None

let map_opt2 ~f opt1 opt2 =
  match opt1, opt2 with
  | Some x, Some y -> Some (f x y)
  | _ -> None

let singleton x_opt =
  map_opt ~f:(fun x -> [x]) x_opt

let merge x_opt xs_opt =
  map_opt2 ~f:(fun x xs -> x :: xs) x_opt xs_opt

let init_hashtbl n kv_pairs =
  let tbl = Hashtbl.create n in
  List.iter (fun (k, v) -> Hashtbl.replace tbl k v) kv_pairs;
  tbl

let is_some_opt opt =
  match opt with
  | Some _ -> true
  | None -> false

let is_none_opt opt =
  is_some_opt opt |> not

let flatten_opt_list opt_list =
  List.fold_left (fun acc opt -> merge opt acc) (Some []) opt_list
