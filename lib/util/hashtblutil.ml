(* hashtblutil.ml *)

open! StdLabels
open! MoreLabels

let init kv_pairs =
  let tbl = (List.length kv_pairs * 2) - 1 |> Hashtbl.create in
  List.iter ~f:(fun (key, data) -> Hashtbl.add tbl ~key ~data) kv_pairs;
  tbl

let add_all tbl1 tbl2 = Hashtbl.iter ~f:(Hashtbl.add tbl1) tbl2

let set_from_list elems =
  let set = Hashtbl.create ((List.length elems * 2) - 1) in
  List.iter ~f:(fun elem -> Hashtbl.add set ~key:elem ~data:()) elems;
  set

let append tbl ~key ~data =
  match Hashtbl.find_opt tbl key with
  | None -> Hashtbl.add tbl ~key ~data:[ data ]
  | Some xs -> Hashtbl.replace tbl ~key ~data:(data :: xs)
