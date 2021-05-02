(* optutil.mli *)

open! StdLabels

val fold2 : none:'c -> some:('a -> 'b -> 'c) -> 'a Option.t -> 'b Option.t -> 'c

val bind2 :
  f:('a -> 'b -> 'c Option.t) -> 'a Option.t -> 'b Option.t -> 'c Option.t

val map2 : f:('a -> 'b -> 'c) -> 'a Option.t -> 'b Option.t -> 'c Option.t

val singleton : 'a Option.t -> 'a List.t Option.t

val merge : 'a Option.t -> 'a List.t Option.t -> 'a List.t Option.t

val flatten_opt_list : 'a Option.t List.t -> 'a List.t Option.t
