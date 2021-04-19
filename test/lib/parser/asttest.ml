(* asttest.ml *)

open Parser
open Util
module Abssyn = Abstractsyntax
module T = Tables

let x = Tables.make_id "x"

let y = Tables.make_id "y"

let startpos =
  { Lexing.pos_fname = "test.cl"; pos_lnum = 15; pos_bol = 3; pos_cnum = 54 }

let endpos =
  { Lexing.pos_fname = "test.cl"; pos_lnum = 16; pos_bol = 10; pos_cnum = 70 }

let loc = (startpos, endpos)

let startpos2 =
  { Lexing.pos_fname = "test.cl"; pos_lnum = 20; pos_bol = 7; pos_cnum = 10 }

let endpos2 =
  { Lexing.pos_fname = "test.cl"; pos_lnum = 21; pos_bol = 42; pos_cnum = 55 }

let loc2 = (startpos2, endpos2)

let startpos3 =
  { Lexing.pos_fname = "test.cl"; pos_lnum = 101; pos_bol = 12; pos_cnum = 15 }

let endpos3 =
  { Lexing.pos_fname = "test.cl"; pos_lnum = 105; pos_bol = 22; pos_cnum = 30 }

let loc3 = (startpos3, endpos3)

let bool_const =
  { Abssyn.expr_expr = Abssyn.BoolConst true; expr_typ = None; expr_loc = loc }

let int_const =
  {
    Abssyn.expr_expr = Abssyn.IntConst (T.make_int "42");
    expr_typ = None;
    expr_loc = loc2;
  }

let var_expr =
  { Abssyn.expr_expr = Abssyn.Variable x; expr_typ = None; expr_loc = loc3 }

let%test "create_expr" =
  Ast.create_expr ~expr:(Abssyn.BoolConst true) loc = bool_const
  && Ast.create_expr ~typ:T.bool_type ~expr:(Abssyn.BoolConst true) loc
     = { bool_const with Abssyn.expr_typ = Some T.bool_type }

let%test "no_expr" =
  Ast.no_expr ~loc = { bool_const with Abssyn.expr_expr = Abssyn.NoExpr }

let%test "replace_expr" =
  Ast.replace_expr ~new_expr:Abssyn.NoExpr bool_const
  = { bool_const with Abssyn.expr_expr = Abssyn.NoExpr }

let%test "add_type" =
  Ast.add_type ~typ:T.bool_type bool_const
  = { bool_const with Abssyn.expr_typ = Some T.bool_type }

let%test "create_let" =
  let bindings =
    [ (y, T.bool_type, bool_const, loc); (x, T.int_type, int_const, loc2) ]
  in
  Ast.create_let ~bindings ~body:var_expr
  = {
      Abssyn.expr_expr =
        Abssyn.Let
          {
            let_var = (x, T.int_type);
            let_init = int_const;
            let_body =
              {
                Abssyn.expr_expr =
                  Abssyn.Let
                    {
                      let_var = (y, T.bool_type);
                      let_init = bool_const;
                      let_body = var_expr;
                    };
                expr_typ = None;
                expr_loc = loc;
              };
          };
      expr_typ = None;
      expr_loc = loc2;
    }
