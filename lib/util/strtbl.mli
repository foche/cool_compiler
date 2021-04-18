(* strtbl.mli *)

type handle
(** A [handle] that identifies a string. Can be used to retrieve the original string. *)

type 'a t
(** The type of string tables. *)

val create : int -> 'a t
(** [create n] creates a string table with an initial size [n]. *)

val add : 'a t -> 'a -> handle
(** [add tbl s] adds string [s] to table [tbl]. Returns the handle for [s]. *)

val find : 'a t -> handle -> 'a
(** [find tbl h] uses handle [h] to look up its associated string. *)

val length : 'a t -> int
