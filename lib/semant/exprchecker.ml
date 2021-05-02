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

let rec typecheck ?(super_typ = Tbls.object_type) ~ctx expr =
  match expr.Abssyn.expr_expr with
  | Abssyn.IntConst _ -> Ast.add_type ~typ:Tbls.int_type expr
  | Abssyn.StrConst _ -> Ast.add_type ~typ:Tbls.string_type expr
  | Abssyn.BoolConst _ -> Ast.add_type ~typ:Tbls.bool_type expr
  | Abssyn.Variable id -> aux_var ~ctx ~expr ~id
  | Abssyn.Assign (id, sub_expr) -> aux_assign ~ctx ~expr ~id ~sub_expr
  | Abssyn.New typ -> aux_new ~ctx ~expr ~typ
  | Abssyn.Cond cond_expr -> aux_cond ~ctx ~expr ~cond_expr
  | Abssyn.Block (stmts, sub_expr) -> aux_block ~ctx ~expr ~stmts ~sub_expr
  | Abssyn.IsVoid sub_expr -> aux_isvoid ~ctx ~expr ~sub_expr
  | Abssyn.Loop loop_expr -> aux_loop ~ctx ~expr ~loop_expr
  | Abssyn.Not sub_expr -> aux_not ~ctx ~expr ~sub_expr
  | Abssyn.Neg sub_expr -> aux_neg ~ctx ~expr ~sub_expr
  | Abssyn.Arith arith_expr -> aux_arith ~ctx ~expr ~arith_expr
  | Abssyn.Comp comp_expr -> aux_comp ~ctx ~expr ~comp_expr
  | Abssyn.Eq (e1, e2) -> aux_eq ~ctx ~expr ~e1 ~e2
  | Abssyn.Let let_expr -> aux_let ~ctx ~expr ~let_expr
  | Abssyn.DynamicDispatch dyn -> aux_dyn_dispatch ~ctx ~expr ~dyn
  | Abssyn.StaticDispatch stat -> aux_stat_dispatch ~ctx ~expr ~stat
  | Abssyn.Case case_expr -> aux_case ~ctx ~expr ~case_expr
  | Abssyn.NoExpr -> Ast.add_type ~typ:super_typ expr

and aux_var ~ctx ~expr ~id =
  match Symtbl.find_opt ctx.id_env id with
  | Some typ -> Ast.add_type ~typ expr
  | None ->
      Semantprint.print_error ~loc:expr.Abssyn.expr_loc
        "Undeclared identifier %a." Tbls.print_id id;
      expr

and aux_assign ~ctx ~expr ~id ~sub_expr =
  let { Abssyn.expr_loc = loc; _ } = expr in
  if id = Tbls.self_var then (
    Semantprint.print_error ~loc "Cannot assign to 'self'.";
    expr)
  else
    match Symtbl.find_opt ctx.id_env id with
    | None ->
        Semantprint.print_error ~loc "Assignment to undeclared variable %a."
          Tbls.print_id id;
        expr
    | Some var_typ ->
        rec_helper ~ctx
          ~cont:(fun ~typed_sub_expr sub_expr_typ ->
            Ast.replace_expr ~expr
              ~new_expr:(Abssyn.Assign (id, typed_sub_expr))
              ~typ:sub_expr_typ)
          ~err_fun:(fun ~sub_expr_typ ->
            Semantprint.print_error ~loc:sub_expr.Abssyn.expr_loc
              "Type %a of assigned expression does not conform to declared \
               type %a of identifier %a."
              Tbls.print_type sub_expr_typ Tbls.print_type var_typ Tbls.print_id
              id)
          ~expr ~sub_expr ~super_typ:var_typ

and aux_new ~ctx ~expr ~typ =
  if Tree.mem ctx.inherit_tree typ then Ast.add_type ~typ expr
  else (
    Semantprint.print_error ~loc:expr.Abssyn.expr_loc
      "'new' used with undefined class %a." Tbls.print_type typ;
    expr)

and aux_cond ~ctx ~expr ~cond_expr:{ Abssyn.cond_pred; cond_true; cond_false } =
  rec_helper ~ctx
    ~cont:(fun ~typed_sub_expr:typed_pred _ ->
      let true_branch = typecheck ~ctx cond_true in
      let false_branch = typecheck ~ctx cond_false in
      Opts.fold2 ~none:expr
        ~some:(fun then_typ else_typ ->
          let cond_typ =
            lca ~inherit_tree:ctx.inherit_tree ~cl_typ:ctx.cl_typ then_typ
              else_typ
          in
          Ast.replace_expr ~expr
            ~new_expr:
              (Abssyn.Cond
                 {
                   Abssyn.cond_pred = typed_pred;
                   cond_true = true_branch;
                   cond_false = false_branch;
                 })
            ~typ:cond_typ)
        true_branch.Abssyn.expr_typ false_branch.Abssyn.expr_typ)
    ~err_fun:(fun ~sub_expr_typ:_ ->
      Semantprint.print_error ~loc:cond_pred.Abssyn.expr_loc
        "Predicate of 'if' does not have type Bool.")
    ~expr ~sub_expr:cond_pred ~super_typ:Tbls.bool_type

and aux_block ~ctx ~expr ~stmts ~sub_expr =
  let typed_stmts = List.map ~f:(typecheck ~ctx) stmts in
  let typed_sub_expr = typecheck ~ctx sub_expr in
  let valid_stmts =
    List.for_all
      ~f:(fun sub_expr -> Option.is_some sub_expr.Abssyn.expr_typ)
      typed_stmts
  in
  if valid_stmts then
    match typed_sub_expr.Abssyn.expr_typ with
    | None -> expr
    | Some sub_expr_typ ->
        Ast.replace_expr ~expr
          ~new_expr:(Abssyn.Block (typed_stmts, typed_sub_expr))
          ~typ:sub_expr_typ
  else expr

and aux_isvoid ~ctx ~expr ~sub_expr =
  let typed_sub_expr = typecheck ~ctx sub_expr in
  match typed_sub_expr.Abssyn.expr_typ with
  | None -> expr
  | Some _ ->
      Ast.replace_expr ~expr ~new_expr:(Abssyn.IsVoid typed_sub_expr)
        ~typ:Tbls.bool_type

and aux_loop ~ctx ~expr ~loop_expr:{ Abssyn.loop_pred; loop_body } =
  rec_helper ~ctx
    ~cont:(fun ~typed_sub_expr:typed_pred _ ->
      let typed_body = typecheck ~ctx loop_body in
      match typed_body.Abssyn.expr_typ with
      | None -> expr
      | Some _ ->
          Ast.replace_expr ~expr
            ~new_expr:
              (Abssyn.Loop
                 { Abssyn.loop_pred = typed_pred; loop_body = typed_body })
            ~typ:Tbls.object_type)
    ~err_fun:(fun ~sub_expr_typ:_ ->
      Semantprint.print_error ~loc:loop_pred.Abssyn.expr_loc
        "Loop condition does not have type Bool.")
    ~expr ~sub_expr:loop_pred ~super_typ:Tbls.bool_type

and aux_not ~ctx ~expr ~sub_expr =
  rec_helper ~ctx
    ~cont:(fun ~typed_sub_expr _ ->
      Ast.replace_expr ~expr ~new_expr:(Abssyn.Not typed_sub_expr)
        ~typ:Tbls.bool_type)
    ~err_fun:(fun ~sub_expr_typ ->
      Semantprint.print_error ~loc:sub_expr.Abssyn.expr_loc
        "Argument of 'not' has type %a instead of Bool." Tbls.print_type
        sub_expr_typ)
    ~expr ~sub_expr ~super_typ:Tbls.bool_type

and aux_neg ~ctx ~expr ~sub_expr =
  rec_helper ~ctx
    ~cont:(fun ~typed_sub_expr _ ->
      Ast.replace_expr ~expr ~new_expr:(Abssyn.Neg typed_sub_expr)
        ~typ:Tbls.int_type)
    ~err_fun:(fun ~sub_expr_typ ->
      Semantprint.print_error ~loc:sub_expr.Abssyn.expr_loc
        "Argument of '~' has type %a instead of Int." Tbls.print_type
        sub_expr_typ)
    ~expr ~sub_expr ~super_typ:Tbls.int_type

and aux_arith ~ctx ~expr ~arith_expr:{ Abssyn.arith_op; arith_e1; arith_e2 } =
  let op_str = get_arith_op_str arith_op in
  aux_binop ~ctx ~expr ~op_str
    ~f:(fun ~typed_e1 ~typed_e2 ->
      Abssyn.Arith { Abssyn.arith_op; arith_e1 = typed_e1; arith_e2 = typed_e2 })
    ~e1:arith_e1 ~e2:arith_e2 ~typ:Tbls.int_type

and aux_comp ~ctx ~expr ~comp_expr:{ Abssyn.comp_op; comp_e1; comp_e2 } =
  let op_str = get_comp_op_str comp_op in
  aux_binop ~ctx ~expr ~op_str
    ~f:(fun ~typed_e1 ~typed_e2 ->
      Abssyn.Comp { Abssyn.comp_op; comp_e1 = typed_e1; comp_e2 = typed_e2 })
    ~e1:comp_e1 ~e2:comp_e2 ~typ:Tbls.bool_type

and aux_binop ~ctx ~expr ~op_str ~f ~e1 ~e2 ~typ =
  let typed_e1 = typecheck ~ctx e1 in
  let typed_e2 = typecheck ~ctx e2 in
  Opts.fold2 ~none:expr
    ~some:(fun typ1 typ2 ->
      let int_args = typ1 = Tbls.int_type && typ2 = Tbls.int_type in
      if int_args then
        Ast.replace_expr ~expr ~new_expr:(f ~typed_e1 ~typed_e2) ~typ
      else (
        Semantprint.print_error ~loc:expr.Abssyn.expr_loc
          "non-Int arguments: %a %s %a" Tbls.print_type typ1 op_str
          Tbls.print_type typ2;
        expr))
    typed_e1.Abssyn.expr_typ typed_e2.Abssyn.expr_typ

and aux_eq ~ctx ~expr ~e1 ~e2 =
  let typed_e1 = typecheck ~ctx e1 in
  let typed_e2 = typecheck ~ctx e2 in
  Opts.fold2 ~none:expr
    ~some:(fun typ1 typ2 ->
      let is_valid_comp =
        ((not @@ Tbls.is_primitive typ1) && (not @@ Tbls.is_primitive typ2))
        || typ1 = typ2
      in
      if is_valid_comp then
        Ast.replace_expr ~expr
          ~new_expr:(Abssyn.Eq (typed_e1, typed_e2))
          ~typ:Tbls.bool_type
      else (
        Semantprint.print_error ~loc:expr.Abssyn.expr_loc
          "Illegal comparison with a basic type.";
        expr))
    typed_e1.Abssyn.expr_typ typed_e2.Abssyn.expr_typ

and validate_let_var_not_self ~loc ~id =
  Validator.bind
    ~checker:(lazy (id <> Tbls.self_var))
    ~err_fun:
      (lazy
        (Semantprint.print_error ~loc
           "'self' cannot be bound in a 'let' expression."))
    true

and validate_let_type ~inherit_tree ~id ~var_typ ~loc =
  Validator.bind
    ~checker:(lazy (Tree.mem inherit_tree var_typ))
    ~err_fun:
      (lazy
        (Semantprint.print_error ~loc
           "Class %a of let-bound identifier %a is undefined." Tbls.print_type
           var_typ Tbls.print_id id))
    true

and aux_let ~ctx ~expr ~let_expr =
  let { Abssyn.let_var = { Abssyn.elem = id, var_typ; _ }; let_init; _ } =
    let_expr
  in
  let loc = expr.Abssyn.expr_loc in
  let is_valid_var = validate_let_var_not_self ~loc ~id in
  let is_valid_type =
    validate_let_type ~inherit_tree:ctx.inherit_tree ~id ~var_typ ~loc
  in
  if is_valid_var && is_valid_type then
    rec_helper ~ctx
      ~cont:(fun ~typed_sub_expr:typed_init _ ->
        aux_let_helper ~ctx ~expr ~let_expr ~typed_init)
      ~err_fun:(fun ~sub_expr_typ ->
        Semantprint.print_error ~loc:let_init.Abssyn.expr_loc
          "Inferred type %a of initialization of %a does not conform to \
           identifier's declared type %a."
          Tbls.print_type sub_expr_typ Tbls.print_id id Tbls.print_type var_typ)
      ~expr ~sub_expr:let_init ~super_typ:var_typ
  else expr

and aux_let_helper ~ctx ~expr ~let_expr ~typed_init =
  Symtbl.enter_scope ctx.id_env
    ~cont:
      (lazy
        (let { Abssyn.let_var = { Abssyn.elem = id, var_typ; _ }; let_body; _ }
             =
           let_expr
         in
         Symtbl.add ctx.id_env ~key:id ~data:var_typ |> ignore;
         let typed_body = typecheck ~ctx let_body in
         match typed_body.Abssyn.expr_typ with
         | None -> expr
         | Some body_typ ->
             Ast.replace_expr ~expr
               ~new_expr:
                 (Abssyn.Let
                    {
                      let_expr with
                      Abssyn.let_init = typed_init;
                      let_body = typed_body;
                    })
               ~typ:body_typ))

and aux_dyn_dispatch ~ctx ~expr ~dyn =
  let { Abssyn.dyn_recv; dyn_method_id; dyn_args; _ } = dyn in
  aux_dispatch_common ~ctx ~expr ~method_id:dyn_method_id ~recv:dyn_recv
    ~args:dyn_args
    ~recv_type_translator:(fun ~recv_typ ->
      Some (Types.translate_type ~cl_typ:ctx.cl_typ recv_typ))
    ~cont:(fun ~typed_recv ~typed_args ~trans_ret_typ ~label:_ ->
      Ast.replace_expr ~expr
        ~new_expr:
          (Abssyn.DynamicDispatch
             { dyn with Abssyn.dyn_recv = typed_recv; dyn_args = typed_args })
        ~typ:trans_ret_typ)

and validate_stat_recv_type ~ctx ~loc ~target_typ ~recv_typ =
  let is_valid_recv_type =
    Types.is_subtype ctx.inherit_tree ~cl_typ:ctx.cl_typ ~sub_typ:recv_typ
      ~super_typ:target_typ
  in
  if is_valid_recv_type then Some target_typ
  else (
    Semantprint.print_error ~loc
      "Expression type %a does not conform to declared static dispatch type %a."
      Tbls.print_type recv_typ Tbls.print_type target_typ;
    None)

and aux_stat_dispatch ~ctx ~expr ~stat =
  let { Abssyn.stat_recv; stat_target_typ; stat_method_id; stat_args; _ } =
    stat
  in
  if Tree.mem ctx.inherit_tree stat_target_typ then
    aux_dispatch_common ~ctx ~expr ~method_id:stat_method_id ~recv:stat_recv
      ~args:stat_args
      ~recv_type_translator:
        (validate_stat_recv_type ~ctx ~loc:expr.Abssyn.expr_loc
           ~target_typ:stat_target_typ)
      ~cont:(fun ~typed_recv ~typed_args ~trans_ret_typ ~label ->
        Ast.replace_expr ~expr
          ~new_expr:
            (Abssyn.StaticDispatch
               {
                 stat with
                 Abssyn.stat_recv = typed_recv;
                 stat_args = typed_args;
                 stat_label = Some label;
               })
          ~typ:trans_ret_typ)
  else (
    Semantprint.print_error ~loc:expr.Abssyn.expr_loc
      "Static dispatch to undefined class %a." Tbls.print_type stat_target_typ;
    expr)

and aux_dispatch_common ~ctx ~expr ~method_id ~recv ~args ~recv_type_translator
    ~cont =
  let typed_recv = typecheck ~ctx recv in
  let typed_args = List.map ~f:(typecheck ~ctx) args in
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
    typed_recv.Abssyn.expr_typ arg_typ_opt

and check_method_sig ~ctx ~expr ~method_id ~typed_recv ~recv_typ ~typed_args
    ~arg_typs ~cont method_sig =
  let { Abssyn.expr_loc = loc; _ } = expr in
  match method_sig with
  | None ->
      Semantprint.print_error ~loc "Dispatch to undefined method %a."
        Tbls.print_id method_id;
      expr
  | Some { Methodtbl.method_ret_typ; formals; label; _ } ->
      if List.compare_lengths typed_args formals <> 0 then (
        Semantprint.print_error ~loc
          "Method %a called with wrong number of arguments." Tbls.print_id
          method_id;
        expr)
      else
        let trans_ret_typ =
          Types.translate_type ~cl_typ:recv_typ method_ret_typ
        in
        let valid_arg_types =
          List.for_all2
            ~f:(validate_arg_type ~ctx ~loc ~method_id)
            arg_typs formals
        in
        if valid_arg_types then
          cont ~typed_recv ~typed_args ~trans_ret_typ ~label
        else expr

and validate_arg_type ~ctx ~loc ~method_id arg_typ (id, formal_typ) =
  Validator.bind
    ~checker:
      (lazy
        (Types.is_subtype ctx.inherit_tree ~cl_typ:ctx.cl_typ ~sub_typ:arg_typ
           ~super_typ:formal_typ))
    ~err_fun:
      (lazy
        (Semantprint.print_error ~loc
           "In call of method %a, type %a of parameter %a does not conform to \
            declared type %a."
           Tbls.print_id method_id Tbls.print_type arg_typ Tbls.print_id id
           Tbls.print_type formal_typ))
    true

and aux_case ~ctx ~expr ~case_expr:{ Abssyn.case_expr; case_branches } =
  let typed_case_expr = typecheck ~ctx case_expr in
  let typed_branches_opt =
    List.fold_right
      ~f:(fun branch -> Opts.merge (aux_branch ~ctx branch))
      case_branches ~init:(Some [])
  in
  let typ_tbl = Hashtbl.create ((List.length case_branches * 2) - 1) in
  let branches_unique =
    List.for_all ~f:(dedup_branches ~typ_tbl) case_branches
  in
  let is_valid =
    branches_unique && Option.is_some typed_case_expr.Abssyn.expr_typ
  in
  if is_valid then
    match typed_branches_opt with
    | Some typed_branches ->
        create_case_expr ~ctx ~expr ~typed_case_expr ~typed_branches
    | None -> expr
  else expr

and create_case_expr ~ctx ~expr ~typed_case_expr ~typed_branches =
  let branch_types =
    List.rev_map
      ~f:
        (fun {
               Abssyn.elem = { Abssyn.branch_body = { Abssyn.expr_typ; _ }; _ };
               _;
             } -> Option.get expr_typ)
      typed_branches
  in
  let case_typ =
    all_lca ~inherit_tree:ctx.inherit_tree ~cl_typ:ctx.cl_typ ~typs:branch_types
  in
  Ast.replace_expr ~expr
    ~new_expr:
      (Abssyn.Case
         { Abssyn.case_expr = typed_case_expr; case_branches = typed_branches })
    ~typ:case_typ

and dedup_branches ~typ_tbl
    {
      Abssyn.elem = { Abssyn.branch_var = { Abssyn.elem = _, var_typ; _ }; _ };
      loc;
    } =
  Validator.bind
    ~accept:(lazy (Hashtbl.add typ_tbl ~key:var_typ ~data:()))
    ~checker:(lazy (Hashtbl.mem typ_tbl var_typ |> not))
    ~err_fun:
      (lazy
        (Semantprint.print_error ~loc
           "Duplicate branch %a in case statement." Tbls.print_type var_typ))
    true

and aux_branch ~ctx branch =
  let { Abssyn.elem; loc } = branch in
  let { Abssyn.branch_var = { Abssyn.elem = id, var_typ; _ }; branch_body; _ } =
    elem
  in
  if validate_branch ~ctx ~loc ~id ~var_typ then
    Symtbl.enter_scope ctx.id_env
      ~cont:
        (lazy
          (Symtbl.add ctx.id_env ~key:id ~data:var_typ |> ignore;
           let typed_body = typecheck ~ctx branch_body in
           match typed_body.Abssyn.expr_typ with
           | None -> None
           | Some _ ->
               Some
                 {
                   branch with
                   Abssyn.elem = { elem with Abssyn.branch_body = typed_body };
                 }))
  else None

and validate_branch ~ctx ~loc ~id ~var_typ =
  Validator.bind
    ~checker:(Validator.is_not_self_var ~id)
    ~err_fun:(lazy (Semantprint.print_error ~loc "'self' bound in 'case'."))
    true
  |> Validator.bind
       ~checker:(Validator.is_not_self_type ~typ:var_typ)
       ~err_fun:
         (lazy
           (Semantprint.print_error ~loc
              "Identifier %a declared with type SELF_TYPE in case branch."
              Tbls.print_id id))
  |> Validator.bind
       ~checker:(lazy (Tree.mem ctx.inherit_tree var_typ))
       ~err_fun:
         (lazy
           (Semantprint.print_error ~loc "Class %a of case branch is undefined."
              Tbls.print_type var_typ))

and rec_helper ~ctx ~cont ~err_fun ~expr ~sub_expr ~super_typ =
  let typed_sub_expr = typecheck ~super_typ ~ctx sub_expr in
  match typed_sub_expr.Abssyn.expr_typ with
  | None -> sub_expr
  | Some sub_expr_typ ->
      let is_valid_sub_expr_type =
        Types.is_subtype ctx.inherit_tree ~cl_typ:ctx.cl_typ
          ~sub_typ:sub_expr_typ ~super_typ
      in
      if is_valid_sub_expr_type then cont ~typed_sub_expr sub_expr_typ
      else (
        err_fun ~sub_expr_typ;
        expr)
