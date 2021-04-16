(* symtbltest.ml *)

open Util

let%test "find and find_opt" =
  let tbl = Symtbl.create 32 in
  Symtbl.add tbl ~k:1 ~v:5 = (false, false)
  && Symtbl.find tbl ~k:1 = 5
  && Symtbl.find_opt tbl ~k:1 = Some 5

let%test "Empty table" =
  let tbl = Symtbl.create 32 in
  try
    Symtbl.find tbl ~k:1 |> ignore ;
    false
  with Not_found -> Symtbl.find_opt tbl ~k:42 = None

let%test "Shadowed and duplicate variables" =
  let tbl = Symtbl.create 32 in
  Symtbl.add tbl ~k:"x" ~v:3 = (false, false)
  && Symtbl.enter_scope tbl
    ~cont:( lazy
      (Symtbl.add tbl ~k:"x" ~v:42 = (false, true)
      && Symtbl.add tbl ~k:"x" ~v:109 = (true, false)
      && Symtbl.find tbl ~k:"x" = 109) )
  && Symtbl.find tbl ~k:"x" = 3

let%test "Nested scopes" =
  let tbl = Symtbl.create 32 in
  Symtbl.add tbl ~k:"x" ~v:42 = (false, false)
  && Symtbl.enter_scope tbl
    ~cont:( lazy
      (Symtbl.add tbl ~k:"y" ~v:42 = (false, false)
      && Symtbl.enter_scope tbl
        ~cont:( lazy
          (Symtbl.add tbl ~k:"x" ~v:1023 = (false, true)
          && Symtbl.add tbl ~k:"z" ~v:10 = (false, false)
          && Symtbl.find tbl ~k:"x" = 1023
          && Symtbl.find tbl ~k:"y" = 42
          && Symtbl.find tbl ~k:"z" = 10) )
      && Symtbl.find tbl ~k:"x" = 42
      && Symtbl.find tbl ~k:"y" = 42
      && Symtbl.find_opt tbl ~k:"z" = None) )
  && Symtbl.find tbl ~k:"x" = 42
  && Symtbl.find_opt tbl ~k:"y" = None
  && Symtbl.find_opt tbl ~k:"z" = None
