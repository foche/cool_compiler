(* tree.ml *)

open StdLabels
open MoreLabels

type 'a t =
  { out_edges: ('a, 'a) Hashtbl.t
  ; parents: ('a, 'a) Hashtbl.t
  ; root: 'a
  ; euler_tour: 'a array
  ; firsts: ('a, int) Hashtbl.t
  ; tbl: Sparsetbl.t }

type 'a create_result = Tree of 'a t | Cycle of 'a | Disconnected of 'a * 'a

type 'a is_tree_result = IsTree | IsCycle of 'a | IsDisconnected of 'a * 'a

let dfs ~out_edges ~visit_fun ~finish_fun ~init ~root =
  let rec aux ~parent state vert =
    let pre_state = visit_fun ~parent ~state ~vert in
    let post_state =
      Hashtbl.find_all out_edges vert |> List.fold_left ~f:(aux ~parent:vert) ~init:pre_state
    in
    finish_fun ~parent ~pre_state ~post_state ~vert
  in
  aux ~parent:root init root

let rec find_root ~parents ~origin ~vert ~parent =
  match parent = origin with
  | true -> IsCycle origin
  | false -> (
    match Hashtbl.find_opt parents parent with
    | None -> IsDisconnected (vert, parent)
    | Some grandparent -> find_root ~parents ~origin ~vert:parent ~parent:grandparent )

let find_disconnected ~parents ~visited ~key:vert ~data:parent acc =
  match acc with
  | Some _ -> acc
  | None -> (
    match Hashtbl.mem visited vert with
    | true -> None
    | false -> Some (find_root ~parents ~origin:vert ~vert ~parent) )

let check_is_tree ~parents ~out_edges ~root =
  let n = Hashtbl.length parents + 1 in
  let visited = Hashtbl.create (2 * n - 1) in
  dfs ~out_edges
    ~visit_fun:(fun ~parent:_ ~state:_ ~vert -> Hashtbl.replace visited ~key:vert ~data:())
    ~finish_fun:(fun ~parent:_ ~pre_state:_ ~post_state:_ ~vert:_ -> ())
    ~init:()
    ~root
  |> ignore ;
  match Hashtbl.length visited = n with
  | true -> IsTree
  | false ->
      Optutil.get (Hashtbl.fold ~f:(find_disconnected ~parents ~visited) parents ~init:None)

let reduce_to_rmq ~out_edges ~euler_tour ~firsts ~depths ~root =
  dfs ~out_edges
    ~visit_fun:(fun ~parent:_ ~state:(i, depth) ~vert ->
      euler_tour.(i) <- vert ;
      depths.(i) <- depth ;
      Hashtbl.replace firsts ~key:vert ~data:i ;
      (i + 1, depth + 1) )
    ~finish_fun:(fun ~parent ~pre_state:_ ~post_state:(i, depth) ~vert:_ ->
      euler_tour.(i) <- parent ;
      depths.(i) <- depth - 2 ;
      (i + 1, depth - 1) )
    ~init:(0, 0)
    ~root
  |> ignore

let create ~parents ~root =
  let n = Hashtbl.length parents + 1 in
  let out_edges = Hashtbl.create (2 * n - 1) in
  Hashtbl.iter ~f:(fun ~key:cl ~data:parent -> Hashtbl.add out_edges ~key:parent ~data:cl) parents ;
  match check_is_tree ~parents ~out_edges ~root with
  | IsCycle vert -> Cycle vert
  | IsDisconnected (vert1, vert2) -> Disconnected (vert1, vert2)
  | IsTree ->
      let euler_tour = Array.make (2 * n) root in
      let firsts = Hashtbl.create (2 * n - 1) in
      let depths = Array.make (2 * n) 0 in
      reduce_to_rmq ~out_edges ~euler_tour ~firsts ~depths ~root ;
      Tree
        { out_edges
        ; parents= Hashtbl.copy parents
        ; root
        ; euler_tour
        ; firsts
        ; tbl= Sparsetbl.create ~data:depths }

let find_out_edges tree = Hashtbl.find_all tree.out_edges

let mem tree vert = tree.root = vert || Hashtbl.mem tree.parents vert

let find_parent_opt tree = Hashtbl.find_opt tree.parents

let lca tree ~vert1 ~vert2 =
  match vert1 = vert2 with
  | true -> vert1
  | false ->
      let i = Hashtbl.find tree.firsts vert1 in
      let j = Hashtbl.find tree.firsts vert2 in
      let lca_index, _ = Sparsetbl.range_min tree.tbl ~left:i ~right:j in
      tree.euler_tour.(lca_index)

let all_lca tree verts =
  List.fold_left ~f:(fun vert1 vert2 -> lca tree ~vert1 ~vert2) ~init:(List.hd verts) (List.tl verts)

let is_ancestor tree ~ancestor vert =
  lca tree ~vert1:ancestor ~vert2:vert = ancestor

let is_leaf tree vert =
  Hashtbl.find_opt tree.out_edges vert |> Optutil.is_none
