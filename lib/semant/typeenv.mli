(* typeenv.mli *)

type ('a, 'b) t

val create : unit -> ('a, 'b) t
(** [create ()] creates a new typing environment. *)

val enter_scope : ('a, 'b) t -> 'c Lazy.t -> 'c
(**
  [enter_scope env f] starts a new scope of variables and calls [f ()].
  After [f] returns, this function cleans up the scope and the variables added
  in this scope automatically. Finally, [enter_scope] returns the result of [f].
 *)

val add : ('a, 'b) t -> 'a -> 'b -> bool * bool

val find : ('a, 'b) t -> 'a -> 'b

val find_opt : ('a, 'b) t -> 'a -> 'b option
