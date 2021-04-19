(* symtbltest.ml *)

open Util

let%test "find and find_opt" =
  let tbl = Symtbl.create 32 in
  Symtbl.add tbl ~key:1 ~data:5 = (false, false)
  && Symtbl.find tbl 1 = 5
  && Symtbl.find_opt tbl 1 = Some 5

let%test "Empty table" =
  let tbl = Symtbl.create 32 in
  try
    Symtbl.find tbl 1 |> ignore;
    false
  with Not_found -> Symtbl.find_opt tbl 42 = None

let%test "Shadowed and duplicate variables" =
  let tbl = Symtbl.create 32 in
  Symtbl.add tbl ~key:"x" ~data:3 = (false, false)
  && Symtbl.enter_scope tbl
       ~cont:
         (lazy
           (Symtbl.add tbl ~key:"x" ~data:42 = (false, true)
           && Symtbl.add tbl ~key:"x" ~data:109 = (true, false)
           && Symtbl.find tbl "x" = 109))
  && Symtbl.find tbl "x" = 3

let%test "Nested scopes" =
  let tbl = Symtbl.create 32 in
  Symtbl.add tbl ~key:"x" ~data:42 = (false, false)
  && Symtbl.enter_scope tbl
       ~cont:
         (lazy
           (Symtbl.add tbl ~key:"y" ~data:42 = (false, false)
           && Symtbl.enter_scope tbl
                ~cont:
                  (lazy
                    (Symtbl.add tbl ~key:"x" ~data:1023 = (false, true)
                    && Symtbl.add tbl ~key:"z" ~data:10 = (false, false)
                    && Symtbl.find tbl "x" = 1023
                    && Symtbl.find tbl "y" = 42
                    && Symtbl.find tbl "z" = 10))
           && Symtbl.find tbl "x" = 42
           && Symtbl.find tbl "y" = 42
           && Symtbl.find_opt tbl "z" = None))
  && Symtbl.find tbl "x" = 42
  && Symtbl.find_opt tbl "y" = None
  && Symtbl.find_opt tbl "z" = None
