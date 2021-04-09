(* tree.ml *)

open Helpers

type 'a t = {
    out_edges : ('a, 'a) Hashtbl.t;
    parents : ('a, 'a) Hashtbl.t;
    root : 'a;
    euler_tour : 'a array;
    firsts : ('a, int) Hashtbl.t;
    rmq : Rmq.t
  }

type 'a create_result =
  | Tree of 'a t
  | Cycle of 'a
  | Disconnected of 'a * 'a

type 'a is_tree_result =
  | IsTree
  | IsCycle of 'a
  | IsDisconnected of 'a * 'a

let dfs ~out_edges ~visit_fun ~finish_fun ~init u =
  let rec aux parent state v =
    let state2 = visit_fun parent state v in
    let state3 =
      Hashtbl.find_all out_edges v |> List.fold_left (aux v) state2 in
    finish_fun parent state2 state3 v
  in
    aux u init u

let rec find_root ~parents ~origin u v =
  match v = origin with
  | true -> IsCycle v
  | false ->
    match Hashtbl.find_opt parents v with
    | None -> IsDisconnected (u, v)
    | Some w -> find_root ~parents ~origin v w

let find_disconnected ~parents ~visited u v acc =
  match acc with
  | Some _ -> acc
  | None ->
    match Hashtbl.mem visited u with
    | true -> None
    | false -> Some (find_root ~parents ~origin:u u v)

let check_is_tree ~parents ~out_edges ~root =
  let n = Hashtbl.length parents + 1 in
  let visited = Hashtbl.create (2 * n) in
  dfs ~out_edges
    ~visit_fun:(fun _ _ v -> Hashtbl.replace visited v ())
    ~finish_fun:(fun _ _ _ _ -> ())
    ~init:()
    root |>
  ignore;
  match Hashtbl.length visited = n with
  | true -> IsTree
  | false ->
    get_opt (Hashtbl.fold (find_disconnected ~parents ~visited) parents None)

let reduce_to_rmq ~out_edges ~euler_tour ~firsts ~depths ~root =
  dfs ~out_edges
    ~visit_fun:(fun _ (i, depth) v ->
      euler_tour.(i) <- v;
      depths.(i) <- depth;
      Hashtbl.replace firsts v i;
      i + 1, depth + 1)
    ~finish_fun:(fun parent _ (i, depth) _ ->
      euler_tour.(i) <- parent;
      depths.(i) <- depth - 2;
      i + 1, depth - 1)
    ~init:(0, 0)
    root |>
  ignore

let create ~parents ~root =
  let n = Hashtbl.length parents + 1 in
  let out_edges = Hashtbl.create (2 * n) in
  Hashtbl.iter (fun cl parent -> Hashtbl.add out_edges parent cl) parents;
  match check_is_tree ~parents ~out_edges ~root with
  | IsCycle u -> Cycle u
  | IsDisconnected (u, v) -> Disconnected (u, v)
  | IsTree ->
    let euler_tour = Array.make (2 * n) root in
    let firsts = Hashtbl.create 32 in
    let depths = Array.make (2 * n) 0 in
    reduce_to_rmq ~out_edges ~euler_tour ~firsts ~depths ~root;
    Tree {
      out_edges;
      parents = Hashtbl.copy parents;
      root;
      euler_tour;
      firsts;
      rmq = Rmq.create depths;
    }

let find_out_edges g =
  Hashtbl.find_all g.out_edges

let mem g u =
  u = g.root || Hashtbl.mem g.parents u

let rec is_ancestor g ancestor node =
  match ancestor = g.root || node = ancestor with
  | true -> true
  | false ->
    match Hashtbl.find_opt g.parents node with
    | None -> false
    | Some parent -> is_ancestor g ancestor parent

let find_parent_opt g =
  Hashtbl.find_opt g.parents

let lca g u v =
  match u = v with
  | true -> u
  | false ->
    let i = Hashtbl.find g.firsts u in
    let j = Hashtbl.find g.firsts v in
    let lca_i, _ = Rmq.find g.rmq i j in
    g.euler_tour.(lca_i)

let all_lca g vlist =
  List.fold_left (lca g) (List.hd vlist) (List.tl vlist)
