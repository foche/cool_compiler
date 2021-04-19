(* optutiltest.ml *)

open Util

let%test "map2" =
  Optutil.map2 ~f:( + ) (Some 42) (Some 15) = Some 57
  && Optutil.map2 ~f:( + ) None (Some 42) = None
  && Optutil.map2 ~f:( + ) None None = None
  && Optutil.map2 ~f:( + ) (Some 1) None = None

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
