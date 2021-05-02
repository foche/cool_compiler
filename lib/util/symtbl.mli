(* symtbl.mli *)

type ('a, 'b) t

val create : int -> ('a, 'b) t
(** [create n] creates a new typing environment. *)

val enter_scope : ('a, 'b) t -> cont:'c Lazy.t -> 'c
(**
  [enter_scope env f] starts a new scope of variables and calls [f ()].
  After [f] returns, this function cleans up the scope and the variables added
  in this scope automatically. Finally, [enter_scope] returns the result of [f].
 *)

val add : ('a, 'b) t -> key:'a -> data:'b -> Bool.t * Bool.t

val find : ('a, 'b) t -> 'a -> 'b

val find_opt : ('a, 'b) t -> 'a -> 'b Option.t
