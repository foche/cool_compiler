(* asttest.ml *)

open Parser
open Util
module Abssyn = Abstractsyntax

let startpos =
  { Lexing.pos_fname = "test.cl"; pos_lnum = 15; pos_bol = 3; pos_cnum = 54 }

let endpos =
  { Lexing.pos_fname = "test.cl"; pos_lnum = 16; pos_bol = 10; pos_cnum = 70 }

let loc = (startpos, endpos)

let const =
  { Abssyn.expr_expr = Abssyn.BoolConst true; expr_typ = None; expr_loc = loc }

let%test "create_expr" =
  Ast.create_expr ~expr:(Abssyn.BoolConst true) loc = const
  && Ast.create_expr ~typ:Tables.bool_type ~expr:(Abssyn.BoolConst true) loc
     = { const with Abssyn.expr_typ = Some Tables.bool_type }
