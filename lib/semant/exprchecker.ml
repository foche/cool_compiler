(* exprchecker.ml *)

open StdLabels
open MoreLabels
open Parser
open Util
open Abstractsyntax
module T = Tables

type context = {
  id_env : (T.id_sym, T.typ_sym) Symtbl.t;
  sigs : Methodtbl.t;
  inherit_tree : T.typ_sym Tree.t;
  cl_typ : T.typ_sym;
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
  | Plus -> "+"
  | Minus -> "-"
  | Mult -> "*"
  | Div -> "/"

let get_comp_op_str = function Lt -> "<" | Le -> "<="

let rec typecheck ?(super_typ = T.object_type) ~ctx expr =
  match expr.expr_expr with
  | IntConst _ -> Ast.add_type ~typ:T.int_type expr
  | StrConst _ -> Ast.add_type ~typ:T.string_type expr
  | BoolConst _ -> Ast.add_type ~typ:T.bool_type expr
  | Variable id -> aux_var ~ctx ~expr ~id
  | Assign (id, sub_expr) -> aux_assign ~ctx ~expr ~id ~sub_expr
  | New typ -> aux_new ~ctx ~expr ~typ
  | Cond cond_expr -> aux_cond ~ctx ~expr ~cond_expr
  | Block (stmts, sub_expr) -> aux_block ~ctx ~expr ~stmts ~sub_expr
  | IsVoid sub_expr -> aux_isvoid ~ctx ~expr ~sub_expr
  | Loop loop_expr -> aux_loop ~ctx ~expr ~loop_expr
  | Not sub_expr -> aux_not ~ctx ~expr ~sub_expr
  | Neg sub_expr -> aux_neg ~ctx ~expr ~sub_expr
  | Arith arith_expr -> aux_arith ~ctx ~expr ~arith_expr
  | Comp comp_expr -> aux_comp ~ctx ~expr ~comp_expr
  | Eq (e1, e2) -> aux_eq ~ctx ~expr ~e1 ~e2
  | Let let_expr -> aux_let ~ctx ~expr ~let_expr
  | DynamicDispatch dyn -> aux_dyn_dispatch ~ctx ~expr ~dyn
  | StaticDispatch stat -> aux_stat_dispatch ~ctx ~expr ~stat
  | Case case_expr -> aux_case ~ctx ~expr ~case_expr
  | NoExpr -> Ast.add_type ~typ:super_typ expr

and aux_var ~ctx ~expr ~id =
  match Symtbl.find_opt ctx.id_env id with
  | Some typ -> Ast.add_type ~typ expr
  | None ->
      Semantprint.print_error ~loc:expr.expr_loc "Undeclared identifier %a."
        T.print_id id;
      expr

and aux_assign ~ctx ~expr ~id ~sub_expr =
  if id = T.self_var then (
    Semantprint.print_error ~loc:expr.expr_loc "Cannot assign to 'self'.";
    expr)
  else
    match Symtbl.find_opt ctx.id_env id with
    | None ->
        Semantprint.print_error ~loc:expr.expr_loc
          "Assignment to undeclared variable %a." T.print_id id;
        expr
    | Some var_typ ->
        rec_helper ~ctx
          ~cont:(fun ~typed_sub_expr sub_expr_typ ->
            Ast.replace_expr ~expr
              ~new_expr:(Assign (id, typed_sub_expr))
              ~typ:sub_expr_typ)
          ~err_fun:(fun ~sub_expr_typ ->
            Semantprint.print_error ~loc:sub_expr.expr_loc
              "Type %a of assigned expression does not conform to declared \
               type %a of identifier %a."
              T.print_type sub_expr_typ T.print_type var_typ T.print_id id)
          ~expr ~sub_expr ~super_typ:var_typ

and aux_new ~ctx ~expr ~typ =
  if Tree.mem ctx.inherit_tree typ then Ast.add_type ~typ expr
  else (
    Semantprint.print_error ~loc:expr.expr_loc
      "'new' used with undefined class %a." T.print_type typ;
    expr)

and aux_cond ~ctx ~expr ~cond_expr:{ cond_pred; cond_true; cond_false } =
  rec_helper ~ctx
    ~cont:(fun ~typed_sub_expr:typed_pred _ ->
      let true_branch = typecheck ~ctx cond_true in
      let false_branch = typecheck ~ctx cond_false in
      Optutil.fold2 ~none:expr
        ~some:(fun then_typ else_typ ->
          let cond_typ =
            lca ~inherit_tree:ctx.inherit_tree ~cl_typ:ctx.cl_typ then_typ
              else_typ
          in
          Ast.replace_expr ~expr
            ~new_expr:
              (Cond
                 {
                   cond_pred = typed_pred;
                   cond_true = true_branch;
                   cond_false = false_branch;
                 })
            ~typ:cond_typ)
        true_branch.expr_typ false_branch.expr_typ)
    ~err_fun:(fun ~sub_expr_typ:_ ->
      Semantprint.print_error ~loc:cond_pred.expr_loc
        "Predicate of 'if' does not have type Bool.")
    ~expr ~sub_expr:cond_pred ~super_typ:T.bool_type

and aux_block ~ctx ~expr ~stmts ~sub_expr =
  let typed_stmts = List.map ~f:(typecheck ~ctx) stmts in
  let typed_sub_expr = typecheck ~ctx sub_expr in
  let valid_stmts =
    List.for_all
      ~f:(fun sub_expr -> Option.is_some sub_expr.expr_typ)
      typed_stmts
  in
  if valid_stmts then
    match typed_sub_expr.expr_typ with
    | Some sub_expr_typ ->
        Ast.replace_expr ~expr
          ~new_expr:(Block (typed_stmts, typed_sub_expr))
          ~typ:sub_expr_typ
    | None -> expr
  else expr

and aux_isvoid ~ctx ~expr ~sub_expr =
  let typed_sub_expr = typecheck ~ctx sub_expr in
  match typed_sub_expr.expr_typ with
  | None -> expr
  | Some _ ->
      Ast.replace_expr ~expr ~new_expr:(IsVoid typed_sub_expr) ~typ:T.bool_type

and aux_loop ~ctx ~expr ~loop_expr:{ loop_pred; loop_body } =
  rec_helper ~ctx
    ~cont:(fun ~typed_sub_expr:typed_pred _ ->
      let typed_body = typecheck ~ctx loop_body in
      match typed_body.expr_typ with
      | None -> expr
      | Some _ ->
          Ast.replace_expr ~expr
            ~new_expr:(Loop { loop_pred = typed_pred; loop_body = typed_body })
            ~typ:T.object_type)
    ~err_fun:(fun ~sub_expr_typ:_ ->
      Semantprint.print_error ~loc:loop_pred.expr_loc
        "Loop condition does not have type Bool.")
    ~expr ~sub_expr:loop_pred ~super_typ:T.bool_type

and aux_not ~ctx ~expr ~sub_expr =
  rec_helper ~ctx
    ~cont:(fun ~typed_sub_expr _ ->
      Ast.replace_expr ~expr ~new_expr:(Not typed_sub_expr) ~typ:T.bool_type)
    ~err_fun:(fun ~sub_expr_typ ->
      Semantprint.print_error ~loc:sub_expr.expr_loc
        "Argument of 'not' has type %a instead of Bool." T.print_type
        sub_expr_typ)
    ~expr ~sub_expr ~super_typ:T.bool_type

and aux_neg ~ctx ~expr ~sub_expr =
  rec_helper ~ctx
    ~cont:(fun ~typed_sub_expr _ ->
      Ast.replace_expr ~expr ~new_expr:(Neg typed_sub_expr) ~typ:T.int_type)
    ~err_fun:(fun ~sub_expr_typ ->
      Semantprint.print_error ~loc:sub_expr.expr_loc
        "Argument of '~' has type %a instead of Int." T.print_type sub_expr_typ)
    ~expr ~sub_expr ~super_typ:T.int_type

and aux_arith ~ctx ~expr ~arith_expr =
  let arith_op = arith_expr.arith_op in
  let op_str = get_arith_op_str arith_op in
  aux_binop ~ctx ~expr ~op_str
    ~f:(fun e1 e2 -> Arith { arith_op; arith_e1 = e1; arith_e2 = e2 })
    ~e1:arith_expr.arith_e1 ~e2:arith_expr.arith_e2 ~typ:T.int_type

and aux_comp ~ctx ~expr ~comp_expr =
  let comp_op = comp_expr.comp_op in
  let op_str = get_comp_op_str comp_op in
  aux_binop ~ctx ~expr ~op_str
    ~f:(fun e1 e2 -> Comp { comp_op; comp_e1 = e1; comp_e2 = e2 })
    ~e1:comp_expr.comp_e1 ~e2:comp_expr.comp_e2 ~typ:T.bool_type

and aux_binop ~ctx ~expr ~op_str ~f ~e1 ~e2 ~typ =
  let typed_e1 = typecheck ~ctx e1 in
  let typed_e2 = typecheck ~ctx e2 in
  Optutil.fold2 ~none:expr
    ~some:(fun typ1 typ2 ->
      let int_args = typ1 = T.int_type && typ2 = T.int_type in
      if int_args then
        Ast.replace_expr ~expr ~new_expr:(f typed_e1 typed_e2) ~typ
      else (
        Semantprint.print_error ~loc:expr.expr_loc "non-Int arguments: %a %s %a"
          T.print_type typ1 op_str T.print_type typ2;
        expr))
    typed_e1.expr_typ typed_e2.expr_typ

and aux_eq ~ctx ~expr ~e1 ~e2 =
  let typed_e1 = typecheck ~ctx e1 in
  let typed_e2 = typecheck ~ctx e2 in
  Optutil.fold2 ~none:expr
    ~some:(fun typ1 typ2 ->
      let is_valid_comp =
        ((not @@ Tables.is_primitive typ1) && (not @@ Tables.is_primitive typ2))
        || typ1 = typ2
      in
      if is_valid_comp then
        Ast.replace_expr ~expr
          ~new_expr:(Eq (typed_e1, typed_e2))
          ~typ:T.bool_type
      else (
        Semantprint.print_error ~loc:expr.expr_loc
          "Illegal comparison with a basic type.";
        expr))
    typed_e1.expr_typ typed_e2.expr_typ

and validate_let_var_not_self ~loc ~id =
  let is_self_var = id = T.self_var in
  if is_self_var then
    Semantprint.print_error ~loc "'self' cannot be bound in a 'let' expression.";
  not is_self_var

and validate_let_type ~inherit_tree ~id ~var_typ ~loc =
  let is_defined = Tree.mem inherit_tree var_typ in
  if not is_defined then
    Semantprint.print_error ~loc
      "Class %a of let-bound identifier %a is undefined." T.print_type var_typ
      T.print_id id;
  is_defined

and aux_let ~ctx ~expr ~let_expr:({ let_var; let_init; _ } as let_expr) =
  let id, var_typ = let_var.elem in
  let loc = expr.expr_loc in
  let is_valid_var = validate_let_var_not_self ~loc ~id in
  let is_valid_type =
    validate_let_type ~inherit_tree:ctx.inherit_tree ~id ~var_typ ~loc
  in
  if is_valid_var && is_valid_type then
    rec_helper ~ctx
      ~cont:(fun ~typed_sub_expr:typed_init _ ->
        aux_let_helper ~ctx ~expr ~let_expr ~typed_init)
      ~err_fun:(fun ~sub_expr_typ ->
        Semantprint.print_error ~loc:let_init.expr_loc
          "Inferred type %a of initialization of %a does not conform to \
           identifier's declared type %a."
          T.print_type sub_expr_typ T.print_id id T.print_type var_typ)
      ~expr ~sub_expr:let_init ~super_typ:var_typ
  else expr

and aux_let_helper ~ctx ~expr ~let_expr:({ let_var; let_body; _ } as let_expr)
    ~typed_init =
  Symtbl.enter_scope ctx.id_env
    ~cont:
      (lazy
        (let id, var_typ = let_var.elem in
         Symtbl.add ctx.id_env ~key:id ~data:var_typ |> ignore;
         let typed_body = typecheck ~ctx let_body in
         match typed_body.expr_typ with
         | None -> expr
         | Some body_typ ->
             Ast.replace_expr ~expr
               ~new_expr:
                 (Let
                    {
                      let_expr with
                      let_init = typed_init;
                      let_body = typed_body;
                    })
               ~typ:body_typ))

and aux_dyn_dispatch ~ctx ~expr ~dyn =
  aux_dispatch_common ~ctx ~expr ~method_id:dyn.dyn_method_id ~recv:dyn.dyn_recv
    ~args:dyn.dyn_args
    ~recv_type_translator:(fun ~recv_typ ->
      Some (Types.translate_type ~cl_typ:ctx.cl_typ recv_typ))
    ~cont:(fun ~typed_recv ~typed_args ~trans_ret_typ ~label:_ ->
      Ast.replace_expr ~expr
        ~new_expr:
          (DynamicDispatch
             { dyn with dyn_recv = typed_recv; dyn_args = typed_args })
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
      T.print_type recv_typ T.print_type target_typ;
    None)

and aux_stat_dispatch ~ctx ~expr ~stat =
  if Tree.mem ctx.inherit_tree stat.stat_target_typ then
    aux_dispatch_common ~ctx ~expr ~method_id:stat.stat_method_id
      ~recv:stat.stat_recv ~args:stat.stat_args
      ~recv_type_translator:
        (validate_stat_recv_type ~ctx ~loc:expr.expr_loc
           ~target_typ:stat.stat_target_typ)
      ~cont:(fun ~typed_recv ~typed_args ~trans_ret_typ ~label ->
        Ast.replace_expr ~expr
          ~new_expr:
            (StaticDispatch
               {
                 stat with
                 stat_recv = typed_recv;
                 stat_args = typed_args;
                 stat_label = Some label;
               })
          ~typ:trans_ret_typ)
  else (
    Semantprint.print_error ~loc:expr.expr_loc
      "Static dispatch to undefined class %a." T.print_type stat.stat_target_typ;
    expr)

and aux_dispatch_common ~ctx ~expr ~method_id ~recv ~args ~recv_type_translator
    ~cont =
  let typed_recv = typecheck ~ctx recv in
  let typed_args = List.map ~f:(typecheck ~ctx) args in
  let arg_typ_opt =
    List.fold_right
      ~f:(fun arg acc -> Optutil.merge arg.expr_typ acc)
      typed_args ~init:(Some [])
  in
  Optutil.fold2 ~none:expr
    ~some:(fun recv_typ arg_typs ->
      match recv_type_translator ~recv_typ with
      | None -> expr
      | Some trans_recv_typ ->
          Methodtbl.find_opt ctx.sigs ~inherit_tree:ctx.inherit_tree
            ~typ:trans_recv_typ ~method_id
          |> check_method_sig ~ctx ~expr ~method_id ~typed_recv ~recv_typ
               ~typed_args ~arg_typs ~cont)
    typed_recv.expr_typ arg_typ_opt

and check_method_sig ~ctx ~expr ~method_id ~typed_recv ~recv_typ ~typed_args
    ~arg_typs ~cont = function
  | None ->
      Semantprint.print_error ~loc:expr.expr_loc
        "Dispatch to undefined method %a." T.print_id method_id;
      expr
  | Some { ret_typ; formals; label; _ } ->
      if List.compare_lengths typed_args formals <> 0 then (
        Semantprint.print_error ~loc:expr.expr_loc
          "Method %a called with wrong number of arguments." T.print_id
          method_id;
        expr)
      else
        let trans_ret_typ = Types.translate_type ~cl_typ:recv_typ ret_typ in
        let valid_arg_types =
          List.for_all2
            ~f:(validate_arg_type ~ctx ~loc:expr.expr_loc ~method_id)
            arg_typs formals
        in
        if valid_arg_types then
          cont ~typed_recv ~typed_args ~trans_ret_typ ~label
        else expr

and validate_arg_type ~ctx ~loc ~method_id arg_typ (id, formal_typ) =
  let is_valid_arg_type =
    Types.is_subtype ctx.inherit_tree ~cl_typ:ctx.cl_typ ~sub_typ:arg_typ
      ~super_typ:formal_typ
  in
  if not is_valid_arg_type then
    Semantprint.print_error ~loc
      "In call of method %a, type %a of parameter %a does not conform to \
       declared type %a."
      T.print_id method_id T.print_type arg_typ T.print_id id T.print_type
      formal_typ;
  is_valid_arg_type

and aux_case ~ctx ~expr ~case_expr:{ case_expr; case_branches } =
  let typed_case_expr = typecheck ~ctx case_expr in
  let typed_branches_opt =
    List.fold_right
      ~f:(fun branch acc -> Optutil.merge (aux_branch ~ctx branch) acc)
      case_branches ~init:(Some [])
  in
  let typ_tbl = Hashtbl.create ((List.length case_branches * 2) - 1) in
  let branches_unique =
    List.for_all ~f:(dedup_branches ~typ_tbl) case_branches
  in
  let is_valid = branches_unique && Option.is_some typed_case_expr.expr_typ in
  if is_valid then
    match typed_branches_opt with
    | Some typed_branches ->
        create_case_expr ~ctx ~expr ~typed_case_expr ~typed_branches
    | None -> expr
  else expr

and create_case_expr ~ctx ~expr ~typed_case_expr ~typed_branches =
  let branch_types =
    List.map
      ~f:(fun { elem = { branch_body; _ }; _ } ->
        Option.get branch_body.expr_typ)
      typed_branches
  in
  let case_typ =
    all_lca ~inherit_tree:ctx.inherit_tree ~cl_typ:ctx.cl_typ ~typs:branch_types
  in
  Ast.replace_expr ~expr
    ~new_expr:
      (Case { case_expr = typed_case_expr; case_branches = typed_branches })
    ~typ:case_typ

and dedup_branches ~typ_tbl { elem = { branch_var; _ }; loc } =
  let _, var_typ = branch_var.elem in
  let is_duplicate = Hashtbl.mem typ_tbl var_typ in
  if is_duplicate then
    Semantprint.print_error ~loc "Duplicate branch %a in case statement."
      T.print_type var_typ
  else Hashtbl.add typ_tbl ~key:var_typ ~data:();
  not is_duplicate

and aux_branch ~ctx ({ elem = { branch_var; branch_body }; loc } as branch) =
  let id, var_typ = branch_var.elem in
  let is_valid_id = validate_branch_id ~loc ~id in
  let is_valid_typ = validate_branch_typ ~ctx ~loc ~id ~var_typ in
  if is_valid_id && is_valid_typ then
    Symtbl.enter_scope ctx.id_env
      ~cont:
        (lazy
          (Symtbl.add ctx.id_env ~key:id ~data:var_typ |> ignore;
           let typed_body = typecheck ~ctx branch_body in
           match typed_body.expr_typ with
           | None -> None
           | Some _ ->
               Some
                 {
                   branch with
                   elem = { branch.elem with branch_body = typed_body };
                 }))
  else None

and validate_branch_id ~loc ~id =
  let is_self_var = id = T.self_var in
  if is_self_var then Semantprint.print_error ~loc "'self' bound in 'case'.";
  not is_self_var

and validate_branch_typ ~ctx ~loc ~id ~var_typ =
  let is_self_type = var_typ = T.self_type in
  if is_self_type then
    Semantprint.print_error ~loc
      "Identifier %a declared with type SELF_TYPE in case branch." T.print_id id;
  (not is_self_type)
  &&
  let is_class_defined = Tree.mem ctx.inherit_tree var_typ in
  if not is_class_defined then
    Semantprint.print_error ~loc "Class %a of case branch is undefined."
      T.print_type var_typ;
  is_class_defined

and rec_helper ~ctx ~cont ~err_fun ~expr ~sub_expr ~super_typ =
  let typed_sub_expr = typecheck ~super_typ ~ctx sub_expr in
  match typed_sub_expr.expr_typ with
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
