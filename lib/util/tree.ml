(* tree.ml *)

open StdLabels
open MoreLabels

type 'a t = {
  out_edges : ('a, 'a list) Hashtbl.t;
  parents : ('a, 'a) Hashtbl.t;
  root : 'a;
  euler_tour : 'a array;
  firsts : ('a, int) Hashtbl.t;
  tbl : Sparsetbl.t;
}

type 'a create_result = Tree of 'a t | Cycle of 'a | Disconnected of 'a * 'a

type 'a is_tree_result = IsTree | IsCycle of 'a | IsDisconnected of 'a * 'a

let dfs ~out_edges ~visit_fun ~finish_fun ~init ~root =
  let rec aux ~parent state vert =
    let pre_state = visit_fun ~parent ~state ~vert in
    let post_state =
      Hashtbl.find_opt out_edges vert
      |> Option.value ~default:[]
      |> List.fold_left ~f:(aux ~parent:vert) ~init:pre_state
    in
    finish_fun ~parent ~pre_state ~post_state ~vert
  in
  aux ~parent:root init root

let rec find_root ~parents ~origin ~vert ~parent =
  let found_cycle = parent = origin in
  if found_cycle then IsCycle origin
  else
    match Hashtbl.find_opt parents parent with
    | None -> IsDisconnected (vert, parent)
    | Some grandparent ->
        find_root ~parents ~origin ~vert:parent ~parent:grandparent

let find_disconnected ~parents ~visited ~key:vert ~data:parent acc =
  match acc with
  | Some _ -> acc
  | None ->
      if Hashtbl.mem visited vert then None
      else Some (find_root ~parents ~origin:vert ~vert ~parent)

let check_is_tree ~parents ~out_edges ~root =
  let n = Hashtbl.length parents + 1 in
  let visited = Hashtbl.create ((2 * n) - 1) in
  dfs ~out_edges
    ~visit_fun:(fun ~parent:_ ~state:_ ~vert ->
      Hashtbl.add visited ~key:vert ~data:())
    ~finish_fun:(fun ~parent:_ ~pre_state:_ ~post_state:_ ~vert:_ -> ())
    ~init:() ~root
  |> ignore;
  if Hashtbl.length visited = n then IsTree
  else
    Hashtbl.fold ~f:(find_disconnected ~parents ~visited) parents ~init:None
    |> Option.get

let reduce_to_rmq ~out_edges ~euler_tour ~firsts ~depths ~root =
  dfs ~out_edges
    ~visit_fun:(fun ~parent:_ ~state:(i, depth) ~vert ->
      euler_tour.(i) <- vert;
      depths.(i) <- depth;
      Hashtbl.add firsts ~key:vert ~data:i;
      (i + 1, depth + 1))
    ~finish_fun:(fun ~parent ~pre_state:_ ~post_state:(i, depth) ~vert:_ ->
      euler_tour.(i) <- parent;
      depths.(i) <- depth - 2;
      (i + 1, depth - 1))
    ~init:(0, 0) ~root
  |> ignore

let create ~parents ~root =
  let n = Hashtbl.length parents + 1 in
  let out_edges = Hashtbl.create ((2 * n) - 1) in
  Hashtbl.iter
    ~f:(fun ~key:cl ~data:parent ->
      Hashtblutil.append out_edges ~key:parent ~data:cl)
    parents;
  match check_is_tree ~parents ~out_edges ~root with
  | IsCycle vert -> Cycle vert
  | IsDisconnected (vert1, vert2) -> Disconnected (vert1, vert2)
  | IsTree ->
      let euler_tour = Array.make (2 * n) root in
      let firsts = Hashtbl.create ((2 * n) - 1) in
      let depths = Array.make (2 * n) 0 in
      reduce_to_rmq ~out_edges ~euler_tour ~firsts ~depths ~root;
      Tree
        {
          out_edges;
          parents = Hashtbl.copy parents;
          root;
          euler_tour;
          firsts;
          tbl = Sparsetbl.create ~data:depths;
        }

let find_out_edges tree key =
  Hashtbl.find_opt tree.out_edges key |> Option.value ~default:[]

let mem tree vert = tree.root = vert || Hashtbl.mem tree.parents vert

let find_parent_opt tree = Hashtbl.find_opt tree.parents

let lca tree ~vert1 ~vert2 =
  if vert1 = vert2 then vert1
  else
    let i = Hashtbl.find tree.firsts vert1 in
    let j = Hashtbl.find tree.firsts vert2 in
    let lca_index, _ = Sparsetbl.range_min tree.tbl ~left:i ~right:j in
    tree.euler_tour.(lca_index)

let is_ancestor tree ~ancestor vert =
  lca tree ~vert1:ancestor ~vert2:vert = ancestor

let is_leaf tree vert =
  match Hashtbl.find_opt tree.out_edges vert with
  | None -> true
  | Some children -> List.compare_length_with children ~len:0 = 0
