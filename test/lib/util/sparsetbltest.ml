(** sparsetbltest.ml *)

open Util
open Sparsetbl

let data = [|
    (* 0   1     2     3     4 *)
    196; -932; -728; -614; -339;
    (* 5   6     7     8    9 *)
    -139; 104; -225; -874; 417;
    (* 10 11   12   13   14 *)
    273; -21; 490; 981; 468;
    (* 15  16    17   18   19 *)
    779; -509; -298; 921; 931|]

let tbl = create data
let _ = data.(1) <- -100000 (* should have no effect *)

let%test _ = range_min tbl 0 19 = (1, -932)
let%test _ = range_min tbl 7 8 = (8, -874)
let%test _ = range_min tbl 19 19 = (19, 931)
let%test _ = range_min tbl 18 10 = (16, -509)
let%test _ = range_min tbl 10 19 = (16, -509)
let%test _ = range_min tbl 1 11 = (1, -932)
let%test _ = range_min tbl 9 16 = (16, -509)
let%test _ =
  try range_min tbl (-1) 1 |> ignore; false with
  | Invalid_argument _ -> true

let%test _ =
  try range_min tbl 0 20 |> ignore; false with
  | Invalid_argument _ -> true
