(* sparsetbltest.ml *)

open Util

let data =
  [|
     196
   ; -932
   ; -728
   ; -614
   ; -339
   ; -139
   ; 104
   ; -225
   ; -874
   ; 417
   ; 273
   ; -21
   ; 490
   ; 981
   ; 468
   ; 779
   ; -509
   ; -298
   ; 921
   ; 931 |]

let tbl = Sparsetbl.create ~data

let _ = data.(1) <- -100000

(* should have no effect *)

let%test _ = Sparsetbl.range_min tbl ~left:0 ~right:19 = (1, -932)

let%test _ = Sparsetbl.range_min tbl ~left:7 ~right:8 = (8, -874)

let%test _ = Sparsetbl.range_min tbl ~left:19 ~right:19 = (19, 931)

let%test _ = Sparsetbl.range_min tbl ~left:18 ~right:10 = (16, -509)

let%test _ = Sparsetbl.range_min tbl ~left:10 ~right:19 = (16, -509)

let%test _ = Sparsetbl.range_min tbl ~left:1 ~right:11 = (1, -932)

let%test _ = Sparsetbl.range_min tbl ~left:9 ~right:16 = (16, -509)

let%test "Negative left" =
  try
    Sparsetbl.range_min tbl ~left:(-1) ~right:1 |> ignore ;
    false
  with Invalid_argument _ -> true

let%test "Out of bounds right" =
  try
    Sparsetbl.range_min tbl ~left:0 ~right:20 |> ignore ;
    false
  with Invalid_argument _ -> true

let%test "Negative right" =
  try
    Sparsetbl.range_min tbl ~left:3 ~right:(-1) |> ignore ;
    false
  with Invalid_argument _ -> true

  let%test "Out of bounds left" =
    try
      Sparsetbl.range_min tbl ~left:20 ~right:0 |> ignore ;
      false
    with Invalid_argument _ -> true
