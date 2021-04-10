(* ast.ml *)

open Util
open Helpers
open Tables

type 'a with_loc = 'a * int

type expr =
  | Assign of id_sym * expr_node
  | DynDispatch of dynamic_dispatch
  | StaticDispatch of static_dispatch
  | Cond of expr_node * expr_node * expr_node
  | Loop of expr_node * expr_node
  | Block of expr_node list
  | Let of let_statement
  | Case of expr_node * branch_node list
  | New of type_sym
  | IsVoid of expr_node
  | Add of expr_node * expr_node
  | Sub of expr_node * expr_node
  | Mult of expr_node * expr_node
  | Div of expr_node * expr_node
  | Neg of expr_node
  | Lt of expr_node * expr_node
  | Le of expr_node * expr_node
  | Eq of expr_node * expr_node
  | Not of expr_node
  | Variable of id_sym
  | IntConst of int_sym
  | StrConst of str_sym
  | BoolConst of bool
  | NoExpr

and typed_expr = {
    typ_expr : expr;
    typ_type : type_sym option;
  }

and expr_node = typed_expr with_loc

and dynamic_dispatch = {
    dyn_recv : expr_node;
    dyn_method : id_sym;
    dyn_args : expr_node list;
  }

and static_dispatch = {
    stat_recv : expr_node;
    stat_type : type_sym;
    stat_method : id_sym;
    stat_args : expr_node list;
  }

and let_statement = {
    let_var : id_sym;
    let_var_type : type_sym;
    let_init : expr_node;
    let_body : expr_node;
  }

and branch_node = (id_sym * type_sym * expr_node) with_loc

type formal = id_sym * type_sym

type formal_node = formal with_loc

type mthd = {
    method_id : id_sym;
    method_args : formal_node list;
    method_ret_type : type_sym;
    method_body : expr_node;
  }

type feature =
  | Method of mthd
  | Field of id_sym * type_sym * expr_node

type clazz = {
    class_type : type_sym;
    class_parent : type_sym;
    class_features : feature with_loc list;
    class_filename : str_sym;
  }

type class_node = clazz with_loc

type class_list = class_node list

type program = class_list with_loc

let make_expr ~exp ~typ = {
    typ_expr = exp;
    typ_type = typ;
  }

let make_untyped_expr ~exp n =
  make_expr ~exp ~typ:None, n

let make_let bindings body =
  let (x, typ, init, n) = List.hd bindings in
  let init =
    make_untyped_expr ~exp:(
      Let {
        let_var = x;
        let_var_type = typ;
        let_init = init;
        let_body = body;
      })
    n in

  let f body' (x', typ', init', n') =
    make_untyped_expr ~exp:(
      Let {
        let_var = x';
        let_var_type = typ';
        let_init = init';
        let_body = body';
      })
      n' in

  List.fold_left f init (List.tl bindings)

let make_single ~f ~x n =
  map_opt ~f:(fun x' -> make_untyped_expr ~exp:(f x') n) x

let make_double ~f ~x ~y n =
  map_opt2 ~f:(fun x' y' -> make_untyped_expr ~exp:(f x' y') n) x y

let no_expr = make_untyped_expr ~exp:NoExpr 0

let add_type (exp, line_number) typ =
  ({exp with typ_type = Some typ}, line_number)

let translate_type cl typ =
  match typ = Tables.self_type with
  | true -> cl.class_type
  | false -> typ

let is_subtype graph cl typ1 typ2 =
  match typ1 = typ2 with
  | true -> true
  | false ->
    match typ2 = Tables.self_type with
    | true -> false
    | false -> translate_type cl typ1 |> Tree.is_ancestor graph typ2

let get_exp_type exp_node =
  (fst exp_node).typ_type
