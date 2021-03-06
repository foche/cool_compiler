(* strtbl.mli *)

type 'a handle
(** A [handle] that identifies a string. Can be used to retrieve the original string. *)

type 'a handle_module = (module Set.OrderedType with type t = 'a)

type ('a, 'b) t
(** The type of string tables. *)

val create : Int.t -> ('a, 'b) t
(** [create n] creates a string table with an initial size [n]. *)

val add : ('a, 'b) t -> 'a -> 'b handle
(** [add tbl s] adds string [s] to table [tbl]. Returns the handle for [s]. *)

val find : ('a, 'b) t -> 'b handle -> 'a
(** [find tbl h] uses handle [h] to look up its associated string. *)

val length : ('a, 'b) t -> Int.t

val handle_module : ('a, 'b) t -> 'b handle handle_module
