(* tree.mli *)

open! MoreLabels
open! StdLabels

type 'a t

type 'a create_result = Tree of 'a t | Cycle of 'a | Disconnected of 'a * 'a

val create : parents:('a, 'a) Hashtbl.t -> root:'a -> 'a create_result

val find_out_edges : 'a t -> 'a -> 'a List.t

val mem : 'a t -> 'a -> Bool.t

val is_ancestor : 'a t -> ancestor:'a -> 'a -> Bool.t

val lca : 'a t -> vert1:'a -> vert2:'a -> 'a

val find_parent_opt : 'a t -> 'a -> 'a Option.t

val is_leaf : 'a t -> 'a -> Bool.t
