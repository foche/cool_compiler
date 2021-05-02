(* hashtblutil.mli *)

open! MoreLabels

val init : ('a * 'b) List.t -> ('a, 'b) Hashtbl.t

val add_all : ('a, 'b) Hashtbl.t -> ('a, 'b) Hashtbl.t -> Unit.t

val set_from_list : 'a List.t -> ('a, Unit.t) Hashtbl.t

val append : ('a, 'b List.t) Hashtbl.t -> key:'a -> data:'b -> Unit.t
