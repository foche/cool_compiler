(* optutiltest.ml *)

open Util

let%test "fold2" =
  let add_or_42 = Optutil.fold2 ~none:42 ~some:( + ) in
  add_or_42 (Some 55) (Some 15) = 70
  && add_or_42 None (Some 102) = 42
  && add_or_42 None None = 42
  && add_or_42 (Some 1) None = 42

let%test "map2" =
  let map_add = Optutil.map2 ~f:( + ) in
  map_add (Some 42) (Some 15) = Some 57
  && map_add None (Some 42) = None
  && map_add None None = None
  && map_add (Some 1) None = None

let%test "singleton" =
  Optutil.singleton (Some 42) = Some [ 42 ] && Optutil.singleton None = None

let%test "merge" =
  Optutil.merge (Some 105) (Some [ 42 ]) = Some [ 105; 42 ]
  && Optutil.merge None (Some []) = None
  && Optutil.merge (Some 42) None = None
  && Optutil.merge None None = None

let%test "flatten_opt_list" =
  Optutil.flatten_opt_list [ Some 42; Some 195; Some 123 ]
  = Some [ 123; 195; 42 ]
  && Optutil.flatten_opt_list [ Some 42; None; Some 123 ] = None
