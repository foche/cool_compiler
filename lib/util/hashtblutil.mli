(* hashtblutil.mli *)

val init : ('a * 'b) list -> ('a, 'b) Hashtbl.t

val add_all : ('a, 'b) Hashtbl.t -> ('a, 'b) Hashtbl.t -> unit

val set_from_list : 'a list -> ('a, unit) Hashtbl.t

val append : ('a, 'b list) Hashtbl.t -> key:'a -> data:'b -> unit
