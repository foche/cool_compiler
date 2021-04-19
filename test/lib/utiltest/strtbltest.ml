(* strtbltest.ml *)

open Util

let%test "add" =
  let tbl = Strtbl.create 0 in
  let hndl1 = Strtbl.add tbl "2" in
  let hndl2 = Strtbl.add tbl "ave123" in
  Strtbl.find tbl hndl1 = "2" && Strtbl.find tbl hndl2 = "ave123"

let%test "Negative size in create" =
  let tbl = Strtbl.create (-1) in
  let hndl1 = Strtbl.add tbl "" in
  let hndl2 = Strtbl.add tbl "ave123" in
  Strtbl.find tbl hndl1 = "" && Strtbl.find tbl hndl2 = "ave123"

let%test "length" =
  let tbl = Strtbl.create 32 in
  Strtbl.add tbl "asdf" |> ignore;
  Strtbl.add tbl "bb" |> ignore;
  Strtbl.length tbl = 2
