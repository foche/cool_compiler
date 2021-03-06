(* asttest.ml *)

open Parser
open Util
module Abssyn = Abstractsyntax
module T = Tables

let x = Tables.make_id "x"

let y = Tables.make_id "y"

let cl_a = Tables.make_type "A"

let startpos =
  { Lexing.pos_fname = "test.cl"; pos_lnum = 15; pos_bol = 3; pos_cnum = 54 }

let endpos =
  { Lexing.pos_fname = "test.cl"; pos_lnum = 16; pos_bol = 10; pos_cnum = 70 }

let loc = (startpos, endpos)

let loc' = Location.create loc

let startpos2 =
  { Lexing.pos_fname = "test.cl"; pos_lnum = 20; pos_bol = 7; pos_cnum = 10 }

let endpos2 =
  { Lexing.pos_fname = "test.cl"; pos_lnum = 21; pos_bol = 42; pos_cnum = 55 }

let loc2 = (startpos2, endpos2)

let loc2' = Location.create loc2

let startpos3 =
  { Lexing.pos_fname = "test.cl"; pos_lnum = 101; pos_bol = 12; pos_cnum = 15 }

let endpos3 =
  { Lexing.pos_fname = "test.cl"; pos_lnum = 105; pos_bol = 22; pos_cnum = 30 }

let loc3 = (startpos3, endpos3)

let loc3' = Location.create loc3

let bool_const =
  { Abssyn.expr_expr = Abssyn.BoolConst true; expr_typ = None; expr_loc = loc' }

let int_const =
  {
    Abssyn.expr_expr = Abssyn.IntConst (T.make_int "42");
    expr_typ = None;
    expr_loc = loc2';
  }

let var_expr =
  { Abssyn.expr_expr = Abssyn.Variable x; expr_typ = None; expr_loc = loc3' }

let%test "create_expr" =
  Ast.create_expr ~loc (Abssyn.BoolConst true) = bool_const
  && Ast.create_expr ~typ:T.bool_type ~loc (Abssyn.BoolConst true)
     = { bool_const with Abssyn.expr_typ = Some T.bool_type }

let%test "no_expr" =
  Ast.no_expr ~loc = { bool_const with Abssyn.expr_expr = Abssyn.NoExpr }

let%test "replace_expr" =
  Ast.replace_expr ~expr:bool_const ~typ:T.bool_type Abssyn.NoExpr
  = {
      Abssyn.expr_expr = Abssyn.NoExpr;
      expr_typ = Some T.bool_type;
      expr_loc = loc';
    }

let%test "add_type" =
  Ast.add_type ~typ:T.bool_type bool_const
  = { bool_const with Abssyn.expr_typ = Some T.bool_type }

let%test "create_let" =
  let bindings =
    [
      ({ Abssyn.elem = (y, T.bool_type); loc = loc' }, bool_const, loc);
      ({ Abssyn.elem = (x, T.int_type); loc = loc2' }, int_const, loc2);
    ]
  in
  Ast.create_let ~bindings ~body:var_expr
  = {
      Abssyn.expr_expr =
        Abssyn.Let
          {
            let_var = { Abssyn.elem = (x, T.int_type); loc = loc2' };
            let_init = int_const;
            let_body =
              {
                Abssyn.expr_expr =
                  Abssyn.Let
                    {
                      let_var = { Abssyn.elem = (y, T.bool_type); loc = loc' };
                      let_init = bool_const;
                      let_body = var_expr;
                    };
                expr_typ = None;
                expr_loc = loc';
              };
          };
      expr_typ = None;
      expr_loc = loc2';
    }

let%test "create_var_node" =
  Ast.create_var_node ~id:"x" ~typ:"A" ~loc
  = { Abssyn.elem = (x, cl_a); loc = loc' }

let%test "self_var_expr" =
  Ast.self_var_expr ~loc
  = {
      Abssyn.expr_expr = Abssyn.Variable Tables.self_var;
      expr_typ = None;
      expr_loc = loc';
    }
