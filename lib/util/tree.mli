(* tree.mli *)

type 'a t

type 'a create_result = Tree of 'a t | Cycle of 'a | Disconnected of 'a * 'a

val create : parents:('a, 'a) Hashtbl.t -> root:'a -> 'a create_result

val find_out_edges : 'a t -> 'a -> 'a list

val mem : 'a t -> 'a -> bool

val is_ancestor : 'a t -> 'a -> 'a -> bool

val lca : 'a t -> 'a -> 'a -> 'a

val all_lca : 'a t -> 'a list -> 'a

val find_parent_opt : 'a t -> 'a -> 'a option
