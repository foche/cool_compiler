(* exprchecker.ml *)

open! StdLabels
open! MoreLabels
module Abssyn = Parser.Abstractsyntax
module Ast = Parser.Ast
module Opts = Util.Optutil
module Symtbl = Util.Symtbl
module Tbls = Util.Tables
module Tree = Util.Tree

type context = {
  id_env : (Tbls.id_sym, Tbls.typ_sym) Symtbl.t;
  sigs : Methodtbl.t;
  inherit_tree : Tbls.typ_sym Tree.t;
  cl_typ : Tbls.typ_sym;
}

let lca ~inherit_tree ~cl_typ typ1 typ2 =
  if typ1 = typ2 then typ1
  else
    Tree.lca inherit_tree
      ~vert1:(Types.translate_type ~cl_typ typ1)
      ~vert2:(Types.translate_type ~cl_typ typ2)

let all_lca ~inherit_tree ~cl_typ ~typs =
  List.fold_left
    ~f:(lca ~inherit_tree ~cl_typ)
    ~init:(List.hd typs) (List.tl typs)

let get_arith_op_str = function
  | Abssyn.Plus -> "+"
  | Abssyn.Minus -> "-"
  | Abssyn.Mult -> "*"
  | Abssyn.Div -> "/"

let get_comp_op_str = function Abssyn.Lt -> "<" | Abssyn.Le -> "<="

let rec aux ?(super_typ = Tbls.object_type) ?(is_tail_pos = false) ~ctx ~cont
    expr =
  (match expr.Abssyn.expr_expr with
  | Abssyn.IntConst _ -> Ast.add_type ~typ:Tbls.int_type expr
  | Abssyn.StrConst _ -> Ast.add_type ~typ:Tbls.string_type expr
  | Abssyn.BoolConst _ -> Ast.add_type ~typ:Tbls.bool_type expr
  | Abssyn.Variable id -> aux_var ~ctx ~expr id
  | Abssyn.Assign (id, sub_expr) -> aux_assign ~ctx ~expr ~id sub_expr
  | Abssyn.New typ -> aux_new ~ctx ~expr typ
  | Abssyn.Cond cond_expr -> aux_cond ~is_tail_pos ~ctx ~expr cond_expr
  | Abssyn.Block (stmts, sub_expr) ->
      aux_block ~is_tail_pos ~ctx ~expr stmts sub_expr
  | Abssyn.IsVoid sub_expr -> aux_isvoid ~ctx ~expr sub_expr
  | Abssyn.Loop loop_expr -> aux_loop ~ctx ~expr loop_expr
  | Abssyn.Not sub_expr -> aux_not ~ctx ~expr sub_expr
  | Abssyn.Neg sub_expr -> aux_neg ~ctx ~expr sub_expr
  | Abssyn.Arith arith_expr -> aux_arith ~ctx ~expr arith_expr
  | Abssyn.Comp comp_expr -> aux_comp ~ctx ~expr comp_expr
  | Abssyn.Eq (e1, e2) -> aux_eq ~ctx ~expr e1 e2
  | Abssyn.Let let_expr -> aux_let ~is_tail_pos ~ctx ~expr let_expr
  | Abssyn.DynamicDispatch dyn -> aux_dyn_dispatch ~is_tail_pos ~ctx ~expr dyn
  | Abssyn.StaticDispatch stat -> aux_stat_dispatch ~is_tail_pos ~ctx ~expr stat
  | Abssyn.Case case_expr -> aux_case ~is_tail_pos ~ctx ~expr case_expr
  | Abssyn.NoExpr -> Ast.add_type ~typ:super_typ expr)
  |> cont

and aux2 ?is_tail_pos ~ctx ~cont e1 e2 =
  aux ?is_tail_pos ~ctx
    ~cont:(fun typed_e1 -> aux ?is_tail_pos ~ctx ~cont:(cont typed_e1) e2)
    e1

and aux_var ~ctx ~expr id =
  match Symtbl.find_opt ctx.id_env id with
  | Some typ -> Ast.add_type ~typ expr
  | None ->
      Semantprint.print_error ~loc:expr.Abssyn.expr_loc
        "Undeclared identifier %a." Tbls.print_id id;
      expr

and aux_assign ~ctx ~expr ~id sub_expr =
  let { Abssyn.expr_loc = loc; _ } = expr in
  let cont =
    lazy
      (match Symtbl.find_opt ctx.id_env id with
      | None ->
          Semantprint.print_error ~loc "Assignment to undeclared variable %a."
            Tbls.print_id id;
          expr
      | Some var_typ ->
          rec_helper ~ctx
            ~cont:(fun ~typed_sub_expr sub_expr_typ ->
              Abssyn.Assign (id, typed_sub_expr)
              |> Ast.replace_expr ~expr ~typ:sub_expr_typ)
            ~err_fun:(fun ~sub_expr_typ ->
              Semantprint.print_error ~loc:sub_expr.Abssyn.expr_loc
                "Type %a of assigned expression does not conform to declared \
                 type %a of identifier %a."
                Tbls.print_type sub_expr_typ Tbls.print_type var_typ
                Tbls.print_id id)
            ~super_typ:var_typ ~expr sub_expr)
  in
  Validator.is_not_self_var id
  |> Validator.fold
       ~err_fun:(lazy (Semantprint.print_error ~loc "Cannot assign to 'self'."))
       ~fail:expr ~success:cont

and aux_new ~ctx ~expr typ =
  Tree.mem ctx.inherit_tree typ
  |> Validator.fold
       ~err_fun:
         (lazy
           (Semantprint.print_error ~loc:expr.Abssyn.expr_loc
              "'new' used with undefined class %a." Tbls.print_type typ))
       ~fail:expr
       ~success:(lazy (Ast.add_type ~typ expr))

and aux_cond ~is_tail_pos ~ctx ~expr { Abssyn.cond_pred; cond_true; cond_false }
    =
  let pred_cont ~typed_sub_expr:typed_pred _ =
    let branch_cont typed_true typed_false =
      Opts.fold2 ~none:expr
        ~some:(fun true_typ false_typ ->
          let cond_typ =
            lca ~inherit_tree:ctx.inherit_tree ~cl_typ:ctx.cl_typ true_typ
              false_typ
          in
          Abssyn.Cond
            {
              Abssyn.cond_pred = typed_pred;
              cond_true = typed_true;
              cond_false = typed_false;
            }
          |> Ast.replace_expr ~expr ~typ:cond_typ)
        typed_true.Abssyn.expr_typ typed_false.Abssyn.expr_typ
    in
    aux2 ~is_tail_pos ~ctx ~cont:branch_cont cond_true cond_false
  in
  rec_helper ~ctx ~cont:pred_cont
    ~err_fun:(fun ~sub_expr_typ:_ ->
      Semantprint.print_error ~loc:cond_pred.Abssyn.expr_loc
        "Predicate of 'if' does not have type Bool.")
    ~super_typ:Tbls.bool_type ~expr cond_pred

and aux_block ~is_tail_pos ~ctx ~expr stmts sub_expr =
  let typed_stmts = List.map ~f:(aux ~ctx ~cont:Fun.id) stmts in
  let cont typed_sub_expr =
    let valid_stmts =
      List.for_all
        ~f:(fun sub_expr -> Option.is_some sub_expr.Abssyn.expr_typ)
        typed_stmts
    in
    if valid_stmts then
      match typed_sub_expr.Abssyn.expr_typ with
      | None -> expr
      | Some sub_expr_typ ->
          Abssyn.Block (typed_stmts, typed_sub_expr)
          |> Ast.replace_expr ~expr ~typ:sub_expr_typ
    else expr
  in
  aux ~is_tail_pos ~ctx ~cont sub_expr

and aux_isvoid ~ctx ~expr sub_expr =
  let update_isvoid typed_sub_expr =
    match typed_sub_expr.Abssyn.expr_typ with
    | None -> expr
    | Some _ ->
        Abssyn.IsVoid typed_sub_expr
        |> Ast.replace_expr ~expr ~typ:Tbls.bool_type
  in
  aux ~ctx ~cont:update_isvoid sub_expr

and aux_loop ~ctx ~expr { Abssyn.loop_pred; loop_body } =
  let update_loop ~typed_pred typed_body =
    match typed_body.Abssyn.expr_typ with
    | None -> expr
    | Some _ ->
        Abssyn.Loop { Abssyn.loop_pred = typed_pred; loop_body = typed_body }
        |> Ast.replace_expr ~expr ~typ:Tbls.object_type
  in
  rec_helper ~ctx
    ~cont:(fun ~typed_sub_expr:typed_pred _ ->
      aux ~ctx ~cont:(update_loop ~typed_pred) loop_body)
    ~err_fun:(fun ~sub_expr_typ:_ ->
      Semantprint.print_error ~loc:loop_pred.Abssyn.expr_loc
        "Loop condition does not have type Bool.")
    ~super_typ:Tbls.bool_type ~expr loop_pred

and aux_not ~ctx ~expr sub_expr =
  rec_helper ~ctx
    ~cont:(fun ~typed_sub_expr _ ->
      Abssyn.Not typed_sub_expr |> Ast.replace_expr ~expr ~typ:Tbls.bool_type)
    ~err_fun:(fun ~sub_expr_typ ->
      Semantprint.print_error ~loc:sub_expr.Abssyn.expr_loc
        "Argument of 'not' has type %a instead of Bool." Tbls.print_type
        sub_expr_typ)
    ~super_typ:Tbls.bool_type ~expr sub_expr

and aux_neg ~ctx ~expr sub_expr =
  rec_helper ~ctx
    ~cont:(fun ~typed_sub_expr _ ->
      Abssyn.Neg typed_sub_expr |> Ast.replace_expr ~expr ~typ:Tbls.int_type)
    ~err_fun:(fun ~sub_expr_typ ->
      Semantprint.print_error ~loc:sub_expr.Abssyn.expr_loc
        "Argument of '~' has type %a instead of Int." Tbls.print_type
        sub_expr_typ)
    ~super_typ:Tbls.int_type ~expr sub_expr

and aux_arith ~ctx ~expr { Abssyn.arith_op; arith_e1; arith_e2 } =
  let op_str = get_arith_op_str arith_op in
  aux_binop ~ctx ~expr ~op_str
    ~f:(fun typed_e1 typed_e2 ->
      Abssyn.Arith { Abssyn.arith_op; arith_e1 = typed_e1; arith_e2 = typed_e2 })
    ~typ:Tbls.int_type arith_e1 arith_e2

and aux_comp ~ctx ~expr { Abssyn.comp_op; comp_e1; comp_e2 } =
  let op_str = get_comp_op_str comp_op in
  aux_binop ~ctx ~expr ~op_str
    ~f:(fun typed_e1 typed_e2 ->
      Abssyn.Comp { Abssyn.comp_op; comp_e1 = typed_e1; comp_e2 = typed_e2 })
    ~typ:Tbls.bool_type comp_e1 comp_e2

and aux_binop ~ctx ~expr ~op_str ~f ~typ e1 e2 =
  let cont typed_e1 typed_e2 =
    Opts.fold2 ~none:expr
      ~some:(fun typ1 typ2 ->
        (typ1 = Tbls.int_type && typ2 = Tbls.int_type)
        |> Validator.fold
             ~err_fun:
               (lazy
                 (Semantprint.print_error ~loc:expr.Abssyn.expr_loc
                    "non-Int arguments: %a %s %a" Tbls.print_type typ1 op_str
                    Tbls.print_type typ2))
             ~fail:expr
             ~success:
               (lazy (f typed_e1 typed_e2 |> Ast.replace_expr ~expr ~typ)))
      typed_e1.Abssyn.expr_typ typed_e2.Abssyn.expr_typ
  in
  aux2 ~ctx ~cont e1 e2

and aux_eq ~ctx ~expr e1 e2 =
  let cont typed_e1 typed_e2 =
    Opts.fold2 ~none:expr
      ~some:(fun typ1 typ2 ->
        (((not (Tbls.is_primitive typ1)) && not (Tbls.is_primitive typ2))
        || typ1 = typ2)
        |> Validator.fold
             ~err_fun:
               (lazy
                 (Semantprint.print_error ~loc:expr.Abssyn.expr_loc
                    "Illegal comparison with a basic type."))
             ~fail:expr
             ~success:
               (lazy
                 (Abssyn.Eq (typed_e1, typed_e2)
                 |> Ast.replace_expr ~expr ~typ:Tbls.bool_type)))
      typed_e1.Abssyn.expr_typ typed_e2.Abssyn.expr_typ
  in
  aux2 ~ctx ~cont e1 e2

and validate_let_var_not_self ~loc id =
  Validator.map
    ~pred:(Validator.is_not_self_var id)
    ~err_fun:
      (lazy
        (Semantprint.print_error ~loc
           "'self' cannot be bound in a 'let' expression."))
    true

and validate_let_type ~inherit_tree ~loc ~id var_typ =
  Validator.map
    ~pred:(Tree.mem inherit_tree var_typ)
    ~err_fun:
      (lazy
        (Semantprint.print_error ~loc
           "Class %a of let-bound identifier %a is undefined." Tbls.print_type
           var_typ Tbls.print_id id))
    true

and aux_let ~is_tail_pos ~ctx ~expr let_expr =
  let { Abssyn.let_var = { Abssyn.elem = id, var_typ; _ }; let_init; _ } =
    let_expr
  in
  let loc = expr.Abssyn.expr_loc in
  let is_valid_var = validate_let_var_not_self ~loc id in
  let is_valid_type =
    validate_let_type ~inherit_tree:ctx.inherit_tree ~loc ~id var_typ
  in
  if is_valid_var && is_valid_type then
    rec_helper ~ctx
      ~cont:(fun ~typed_sub_expr:typed_init _ ->
        aux_let_helper ~is_tail_pos ~ctx ~expr ~let_expr ~typed_init)
      ~err_fun:(fun ~sub_expr_typ:init_typ ->
        Semantprint.print_error ~loc:let_init.Abssyn.expr_loc
          "Inferred type %a of initialization of %a does not conform to \
           identifier's declared type %a."
          Tbls.print_type init_typ Tbls.print_id id Tbls.print_type var_typ)
      ~super_typ:var_typ ~expr let_init
  else expr

and aux_let_helper ~is_tail_pos ~ctx ~expr ~let_expr ~typed_init =
  let update_let typed_body =
    match typed_body.Abssyn.expr_typ with
    | None -> expr
    | Some body_typ ->
        Abssyn.Let
          { let_expr with Abssyn.let_init = typed_init; let_body = typed_body }
        |> Ast.replace_expr ~expr ~typ:body_typ
  in
  let cont =
    lazy
      (let { Abssyn.let_var = { Abssyn.elem = id, var_typ; _ }; let_body; _ } =
         let_expr
       in
       Symtbl.add ctx.id_env ~key:id ~data:var_typ |> ignore;
       aux ~is_tail_pos ~ctx ~cont:update_let let_body)
  in
  Symtbl.enter_scope ctx.id_env ~cont

and aux_dyn_dispatch ~is_tail_pos ~ctx ~expr dyn =
  let { Abssyn.dyn_recv; dyn_method_id; dyn_args; _ } = dyn in
  let update_dyn ~typed_recv ~typed_args ~trans_ret_typ ~label:_ =
    Abssyn.DynamicDispatch
      {
        dyn with
        Abssyn.dyn_recv = typed_recv;
        dyn_args = typed_args;
        dyn_is_tail = is_tail_pos;
      }
    |> Ast.replace_expr ~expr ~typ:trans_ret_typ
  in
  aux_dispatch_common ~ctx ~expr ~method_id:dyn_method_id ~recv:dyn_recv
    ~args:dyn_args
    ~recv_type_translator:(fun ~recv_typ ->
      Some (Types.translate_type ~cl_typ:ctx.cl_typ recv_typ))
    ~cont:update_dyn

and validate_stat_recv_type ~ctx ~loc ~target_typ ~recv_typ =
  Types.is_subtype ctx.inherit_tree ~cl_typ:ctx.cl_typ ~sub_typ:recv_typ
    ~super_typ:target_typ
  |> Validator.fold
       ~err_fun:
         (lazy
           (Semantprint.print_error ~loc
              "Expression type %a does not conform to declared static dispatch \
               type %a."
              Tbls.print_type recv_typ Tbls.print_type target_typ))
       ~fail:None
       ~success:(lazy (Some target_typ))

and aux_stat_dispatch ~is_tail_pos ~ctx ~expr stat =
  let { Abssyn.stat_recv; stat_target_typ; stat_method_id; stat_args; _ } =
    stat
  in
  let update_stat ~typed_recv ~typed_args ~trans_ret_typ ~label =
    Abssyn.StaticDispatch
      {
        stat with
        Abssyn.stat_recv = typed_recv;
        stat_args = typed_args;
        stat_label = Some label;
        stat_is_tail = is_tail_pos;
      }
    |> Ast.replace_expr ~expr ~typ:trans_ret_typ
  in
  let cont =
    lazy
      (aux_dispatch_common ~ctx ~expr ~method_id:stat_method_id ~recv:stat_recv
         ~args:stat_args
         ~recv_type_translator:
           (validate_stat_recv_type ~ctx ~loc:expr.Abssyn.expr_loc
              ~target_typ:stat_target_typ)
         ~cont:update_stat)
  in
  Tree.mem ctx.inherit_tree stat_target_typ
  |> Validator.fold
       ~err_fun:
         (lazy
           (Semantprint.print_error ~loc:expr.Abssyn.expr_loc
              "Static dispatch to undefined class %a." Tbls.print_type
              stat_target_typ))
       ~fail:expr ~success:cont

and aux_dispatch_common ~ctx ~expr ~method_id ~recv ~args ~recv_type_translator
    ~cont =
  aux ~ctx
    ~cont:(fun typed_recv ->
      let typed_args = List.map ~f:(aux ~ctx ~cont:Fun.id) args in
      let arg_typ_opt =
        List.fold_right
          ~f:(fun arg -> Opts.merge arg.Abssyn.expr_typ)
          typed_args ~init:(Some [])
      in
      Opts.fold2 ~none:expr
        ~some:(fun recv_typ arg_typs ->
          match recv_type_translator ~recv_typ with
          | None -> expr
          | Some trans_recv_typ ->
              Methodtbl.find_opt ctx.sigs ~inherit_tree:ctx.inherit_tree
                ~cl_typ:trans_recv_typ ~method_id
              |> check_method_sig ~ctx ~expr ~method_id ~typed_recv ~recv_typ
                   ~typed_args ~arg_typs ~cont)
        typed_recv.Abssyn.expr_typ arg_typ_opt)
    recv

and check_method_sig ~ctx ~expr ~method_id ~typed_recv ~recv_typ ~typed_args
    ~arg_typs ~cont method_sig =
  let { Abssyn.expr_loc = loc; _ } = expr in
  match method_sig with
  | None ->
      Semantprint.print_error ~loc "Dispatch to undefined method %a."
        Tbls.print_id method_id;
      expr
  | Some { Methodtbl.method_ret_typ; formals; label; _ } ->
      let validate_arg_types =
        lazy
          (let trans_ret_typ =
             Types.translate_type ~cl_typ:recv_typ method_ret_typ
           in
           let valid_arg_types =
             List.for_all2
               ~f:(validate_arg_type ~ctx ~loc ~method_id)
               arg_typs formals
           in
           if valid_arg_types then
             cont ~typed_recv ~typed_args ~trans_ret_typ ~label
           else expr)
      in
      List.compare_lengths typed_args formals
      = 0
      |> Validator.fold
           ~err_fun:
             (lazy
               (Semantprint.print_error ~loc
                  "Method %a called with wrong number of arguments."
                  Tbls.print_id method_id))
           ~fail:expr ~success:validate_arg_types

and validate_arg_type ~ctx ~loc ~method_id arg_typ (id, formal_typ) =
  Validator.map
    ~pred:
      (Types.is_subtype ctx.inherit_tree ~cl_typ:ctx.cl_typ ~sub_typ:arg_typ
         ~super_typ:formal_typ)
    ~err_fun:
      (lazy
        (Semantprint.print_error ~loc
           "In call of method %a, type %a of parameter %a does not conform to \
            declared type %a."
           Tbls.print_id method_id Tbls.print_type arg_typ Tbls.print_id id
           Tbls.print_type formal_typ))
    true

and aux_case ~is_tail_pos ~ctx ~expr { Abssyn.case_expr; case_branches } =
  aux ~ctx
    ~cont:(fun typed_case_expr ->
      let typed_branches_opt =
        List.fold_right
          ~f:(fun branch -> Opts.merge (aux_branch ~is_tail_pos ~ctx branch))
          case_branches ~init:(Some [])
      in
      let typ_tbl = Hashtbl.create ((List.length case_branches * 2) - 1) in
      let branches_unique =
        List.for_all ~f:(dedup_branches ~typ_tbl) case_branches
      in
      if branches_unique && Option.is_some typed_case_expr.Abssyn.expr_typ then
        match typed_branches_opt with
        | None -> expr
        | Some typed_branches ->
            create_case_expr ~ctx ~expr ~typed_case_expr ~typed_branches
      else expr)
    case_expr

and create_case_expr ~ctx ~expr ~typed_case_expr ~typed_branches =
  let case_branches, branch_types = List.split typed_branches in
  let case_typ =
    all_lca ~inherit_tree:ctx.inherit_tree ~cl_typ:ctx.cl_typ ~typs:branch_types
  in
  Abssyn.Case { Abssyn.case_expr = typed_case_expr; case_branches }
  |> Ast.replace_expr ~expr ~typ:case_typ

and dedup_branches ~typ_tbl
    {
      Abssyn.elem = { Abssyn.branch_var = { Abssyn.elem = _, var_typ; _ }; _ };
      loc;
    } =
  Validator.map
    ~accept:(lazy (Hashtbl.add typ_tbl ~key:var_typ ~data:()))
    ~pred:(Hashtbl.mem typ_tbl var_typ |> not)
    ~err_fun:
      (lazy
        (Semantprint.print_error ~loc "Duplicate branch %a in case statement."
           Tbls.print_type var_typ))
    true

and aux_branch ~is_tail_pos ~ctx branch =
  let { Abssyn.elem; loc } = branch in
  let { Abssyn.branch_var = { Abssyn.elem = id, var_typ; _ }; branch_body; _ } =
    elem
  in
  if validate_branch ~ctx ~loc ~id ~var_typ then
    let cont =
      lazy
        (Symtbl.add ctx.id_env ~key:id ~data:var_typ |> ignore;
         let typed_body = aux ~is_tail_pos ~ctx ~cont:Fun.id branch_body in
         match typed_body.Abssyn.expr_typ with
         | None -> None
         | Some body_typ ->
             Some
               ( {
                   branch with
                   Abssyn.elem = { elem with Abssyn.branch_body = typed_body };
                 },
                 body_typ ))
    in
    Symtbl.enter_scope ctx.id_env ~cont
  else None

and validate_branch ~ctx ~loc ~id ~var_typ =
  Validator.map
    ~pred:(Validator.is_not_self_var id)
    ~err_fun:(lazy (Semantprint.print_error ~loc "'self' bound in 'case'."))
    true
  |> Validator.map
       ~pred:(Validator.is_not_self_type var_typ)
       ~err_fun:
         (lazy
           (Semantprint.print_error ~loc
              "Identifier %a declared with type SELF_TYPE in case branch."
              Tbls.print_id id))
  |> Validator.map
       ~pred:(Tree.mem ctx.inherit_tree var_typ)
       ~err_fun:
         (lazy
           (Semantprint.print_error ~loc "Class %a of case branch is undefined."
              Tbls.print_type var_typ))

and rec_helper ~super_typ ?is_tail_pos ~ctx ~cont ~err_fun ~expr sub_expr =
  aux ~super_typ ?is_tail_pos ~ctx
    ~cont:(fun typed_sub_expr ->
      match typed_sub_expr.Abssyn.expr_typ with
      | None -> sub_expr
      | Some sub_expr_typ ->
          Types.is_subtype ctx.inherit_tree ~cl_typ:ctx.cl_typ
            ~sub_typ:sub_expr_typ ~super_typ
          |> Validator.fold
               ~err_fun:(lazy (err_fun ~sub_expr_typ))
               ~fail:expr
               ~success:(lazy (cont ~typed_sub_expr sub_expr_typ)))
    sub_expr

let typecheck ?super_typ ~is_tail_pos ~ctx expr =
  aux ?super_typ ~is_tail_pos ~ctx ~cont:Fun.id expr
