(* exprchecker.ml *)

open StdLabels
open MoreLabels
open Parser
open Util
open Abstractsyntax
module T = Tables

type context = {
  id_env : (T.id_sym, T.type_sym) Symtbl.t;
  sigs : Methodtbl.t;
  inherit_tree : T.type_sym Tree.t;
  cl_typ : T.type_sym;
}

let lca ~inherit_tree ~cl_typ typ1 typ2 =
  match typ1 = typ2 with
  | true -> typ1
  | false ->
      Tree.lca inherit_tree
        ~vert1:(Types.translate_type ~cl_typ typ1)
        ~vert2:(Types.translate_type ~cl_typ typ2)

let all_lca ~inherit_tree ~cl_typ ~typs =
  List.fold_left ~f:(lca ~inherit_tree ~cl_typ) ~init:(List.hd typs) (List.tl typs)

let validate_let_var_not_self ~loc ~id =
  match id <> T.self_var with
  | true -> true
  | false ->
      Semantprint.print_location loc;
      prerr_endline "'self' cannot be bound in a 'let' expression.";
      false

let validate_let_type ~loc ~inherit_tree ~id ~var_typ =
  match Tree.mem inherit_tree var_typ with
  | true -> true
  | false ->
      Semantprint.print_location loc;
      Printf.eprintf "Class %a of let-bound identifier %a is undefined.\n"
        T.print_type var_typ T.print_id id;
      false

let get_arith_op_str op =
  match op with Plus -> "+" | Minus -> "-" | Mult -> "*" | Div -> "/"

let get_comp_op_str op = match op with Lt -> "<" | Le -> "<="

let rec aux ~ctx expr =
  match expr.expr_expr with
  | IntConst _ -> Ast.add_type ~typ:T.int_type expr
  | StrConst _ -> Ast.add_type ~typ:T.string_type expr
  | BoolConst _ -> Ast.add_type ~typ:T.bool_type expr
  | Variable id -> aux_var ~ctx ~expr ~id
  | Assign (id, sub_expr) -> aux_assign ~ctx ~expr ~id ~sub_expr
  | New typ -> aux_new ~ctx ~expr ~typ
  | Cond cond_expr -> aux_cond ~ctx ~expr ~cond_expr
  | Block block -> aux_block ~ctx ~expr ~block
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
  | NoExpr -> invalid_arg "Reached empty init expression in typecheck stage"

and aux_var ~ctx ~expr ~id =
  match Symtbl.find_opt ctx.id_env id with
  | Some typ -> Ast.add_type ~typ expr
  | None ->
      Semantprint.print_location expr.expr_loc;
      Printf.eprintf "Undeclared identifier %a.\n" T.print_id id;
      expr

and aux_assign ~ctx ~expr ~id ~sub_expr =
  match id = T.self_var with
  | true ->
      Semantprint.print_location expr.expr_loc;
      prerr_endline "Cannot assign to 'self'.\n";
      expr
  | false -> (
      match Symtbl.find_opt ctx.id_env id with
      | None ->
          Semantprint.print_location expr.expr_loc;
          Printf.eprintf "Assignment to undeclared variable %a.\n" T.print_id id;
          expr
      | Some var_typ ->
          rec_helper ~ctx
            ~cont:(fun ~typed_sub_expr sub_expr_typ ->
              Ast.replace_expr ~new_expr:(Assign (id, typed_sub_expr)) expr
              |> Ast.add_type ~typ:sub_expr_typ)
            ~err_fun:(fun ~sub_expr_typ ->
              Semantprint.print_location expr.expr_loc;
              Printf.eprintf
                "Type %a of assigned expression does not conform to declared \
                 type %a of identifier %a.\n"
                T.print_type sub_expr_typ T.print_type var_typ T.print_id id)
            ~expr ~sub_expr ~super_typ:var_typ)

and aux_new ~ctx ~expr ~typ =
  match Tree.mem ctx.inherit_tree typ with
  | true -> Ast.add_type ~typ expr
  | false ->
      Semantprint.print_location expr.expr_loc;
      Printf.eprintf "'new' used with undefined class %a.\n" T.print_type typ;
      expr

and aux_cond ~ctx ~expr ~cond_expr =
  rec_helper ~ctx
    ~cont:(fun ~typed_sub_expr:typed_pred _ ->
      let true_branch = aux ~ctx cond_expr.cond_true in
      let false_branch = aux ~ctx cond_expr.cond_false in
      match (true_branch.expr_typ, false_branch.expr_typ) with
      | Some then_typ, Some else_typ ->
          Ast.replace_expr
            ~new_expr:
              (Cond
                 {
                   cond_pred = typed_pred;
                   cond_true = true_branch;
                   cond_false = false_branch;
                 })
            expr
          |> Ast.add_type
               ~typ:
                 (lca ~inherit_tree:ctx.inherit_tree ~cl_typ:ctx.cl_typ then_typ
                    else_typ)
      | _ -> expr)
    ~err_fun:(fun ~sub_expr_typ:_ ->
      Semantprint.print_location cond_expr.cond_pred.expr_loc;
      prerr_endline "Predicate of 'if' does not have type Bool.")
    ~expr ~sub_expr:cond_expr.cond_pred ~super_typ:T.bool_type

and aux_block ~ctx ~expr ~block =
  let typed_block = List.map ~f:(aux ~ctx) block in
  match
    List.for_all
      ~f:(fun sub_expr -> Option.is_some sub_expr.expr_typ)
      typed_block
  with
  | true ->
      let last_expr = List.rev typed_block |> List.hd in
      Ast.replace_expr ~new_expr:(Block typed_block) expr
      |> Ast.add_type ~typ:(Option.get last_expr.expr_typ)
  | false -> expr

and aux_isvoid ~ctx ~expr ~sub_expr =
  let typed_sub_expr = aux ~ctx sub_expr in
  match typed_sub_expr.expr_typ with
  | None -> expr
  | Some _ ->
      Ast.replace_expr ~new_expr:(IsVoid typed_sub_expr) expr
      |> Ast.add_type ~typ:T.bool_type

and aux_loop ~ctx ~expr ~loop_expr =
  rec_helper ~ctx
    ~cont:(fun ~typed_sub_expr:typed_pred _ ->
      let typed_body = aux ~ctx loop_expr.loop_body in
      match typed_body.expr_typ with
      | None -> expr
      | Some _ ->
          Ast.replace_expr
            ~new_expr:(Loop { loop_pred = typed_pred; loop_body = typed_body })
            expr
          |> Ast.add_type ~typ:T.object_type)
    ~err_fun:(fun ~sub_expr_typ:_ ->
      Semantprint.print_location loop_expr.loop_pred.expr_loc;
      prerr_endline "Loop condition does not have type Bool.")
    ~expr ~sub_expr:loop_expr.loop_pred ~super_typ:T.bool_type

and aux_not ~ctx ~expr ~sub_expr =
  rec_helper ~ctx
    ~cont:(fun ~typed_sub_expr _ ->
      Ast.replace_expr ~new_expr:(Not typed_sub_expr) expr
      |> Ast.add_type ~typ:T.bool_type)
    ~err_fun:(fun ~sub_expr_typ ->
      Semantprint.print_location sub_expr.expr_loc;
      Printf.eprintf "Argument of 'not' has type %a instead of Bool.\n"
        T.print_type sub_expr_typ)
    ~expr ~sub_expr ~super_typ:T.bool_type

and aux_neg ~ctx ~expr ~sub_expr =
  rec_helper ~ctx
    ~cont:(fun ~typed_sub_expr _ ->
      Ast.replace_expr ~new_expr:(Neg typed_sub_expr) expr
      |> Ast.add_type ~typ:T.int_type)
    ~err_fun:(fun ~sub_expr_typ ->
      Semantprint.print_location sub_expr.expr_loc;
      Printf.eprintf "Argument of '~' has type %a instead of Bool.\n"
        T.print_type sub_expr_typ)
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
  let typed_e1 = aux ~ctx e1 in
  let typed_e2 = aux ~ctx e2 in
  match (typed_e1.expr_typ, typed_e2.expr_typ) with
  | Some typ1, Some typ2 -> (
      match typ1 = T.int_type && typ2 = T.int_type with
      | true ->
          Ast.replace_expr ~new_expr:(f typed_e1 typed_e2) expr
          |> Ast.add_type ~typ
      | false ->
          Semantprint.print_location expr.expr_loc;
          Printf.eprintf "non-Int arguments: %a %s %a\n" T.print_type typ1
            op_str T.print_type typ2;
          expr)
  | _ -> expr

and aux_eq ~ctx ~expr ~e1 ~e2 =
  let typed_e1 = aux ~ctx e1 in
  let typed_e2 = aux ~ctx e2 in
  match (typed_e1.expr_typ, typed_e2.expr_typ) with
  | Some typ1, Some typ2 -> (
      let primitives = [ T.int_type; T.string_type; T.bool_type ] in
      let typ1_primitive = List.mem typ1 ~set:primitives in
      let typ2_primitive = List.mem typ2 ~set:primitives in
      match ((not typ1_primitive) && not typ2_primitive) || typ1 = typ2 with
      | true ->
          Ast.replace_expr ~new_expr:(Eq (typed_e1, typed_e2)) expr
          |> Ast.add_type ~typ:T.bool_type
      | false ->
          Semantprint.print_location expr.expr_loc;
          prerr_endline "Illegal comparison with a basic type.";
          expr)
  | _ -> expr

and aux_let ~ctx ~expr ~let_expr =
  let id, var_typ = let_expr.let_var in
  let let_init = let_expr.let_init in
  let loc = expr.expr_loc in
  let is_valid_var = validate_let_var_not_self ~loc ~id in
  let is_valid_type =
    validate_let_type ~loc ~inherit_tree:ctx.inherit_tree ~id ~var_typ
  in
  match (is_valid_var && is_valid_type, let_init.expr_expr) with
  | false, _ -> expr
  | true, NoExpr -> aux_let_helper ~ctx ~expr ~let_expr ~typed_init:let_init
  | true, _ ->
      rec_helper ~ctx
        ~cont:(fun ~typed_sub_expr:typed_init _ ->
          aux_let_helper ~ctx ~expr ~let_expr ~typed_init)
        ~err_fun:(fun ~sub_expr_typ ->
          Semantprint.print_location let_init.expr_loc;
          Printf.eprintf
            "Inferred type %a of initialization of %a does not conform to \
             identifier's declared type %a.\n"
            T.print_type sub_expr_typ T.print_id id T.print_type var_typ)
        ~expr ~sub_expr:let_init ~super_typ:var_typ

and aux_let_helper ~ctx ~expr ~let_expr ~typed_init =
  Symtbl.enter_scope ctx.id_env
    ~cont:
      (lazy
        (let id, var_typ = let_expr.let_var in
         Symtbl.add ctx.id_env ~key:id ~data:var_typ |> ignore;
         let typed_body = aux ~ctx let_expr.let_body in
         match typed_body.expr_typ with
         | None -> expr
         | Some body_typ ->
             Ast.replace_expr
               ~new_expr:
                 (Let
                    {
                      let_expr with
                      let_init = typed_init;
                      let_body = typed_body;
                    })
               expr
             |> Ast.add_type ~typ:body_typ))

and aux_dyn_dispatch ~ctx ~expr ~dyn =
  aux_dispatch_common ~ctx ~expr ~method_id:dyn.dyn_method_id ~recv:dyn.dyn_recv
    ~args:dyn.dyn_args
    ~recv_type_translator:(fun ~recv_typ ->
      Some (Types.translate_type ~cl_typ:ctx.cl_typ recv_typ))
    ~cont:(fun ~typed_recv ~typed_args ~ret_typ ~label:_ ->
      Ast.replace_expr
        ~new_expr:
          (DynamicDispatch
             { dyn with dyn_recv = typed_recv; dyn_args = typed_args })
        expr
      |> Ast.add_type ~typ:ret_typ)

and validate_stat_recv_type ~ctx ~loc ~target_typ ~recv_typ =
  match
    Types.is_subtype ctx.inherit_tree ~cl_typ:ctx.cl_typ ~sub_typ:recv_typ
      ~super_typ:target_typ
  with
  | true -> Some target_typ
  | false ->
      Semantprint.print_location loc;
      Printf.eprintf
        "Expression type %a does not conform to declared static dispatch type \
         %a.\n"
        T.print_type recv_typ T.print_type target_typ;
      None

and aux_stat_dispatch ~ctx ~expr ~stat =
  match Tree.mem ctx.inherit_tree stat.stat_target_typ with
  | false ->
      Semantprint.print_location expr.expr_loc;
      Printf.eprintf "Static dispatch to undefined class %a.\n" T.print_type
        stat.stat_target_typ;
      expr
  | true ->
      aux_dispatch_common ~ctx ~expr ~method_id:stat.stat_method_id
        ~recv:stat.stat_recv ~args:stat.stat_args
        ~recv_type_translator:
          (validate_stat_recv_type ~ctx ~loc:expr.expr_loc
             ~target_typ:stat.stat_target_typ)
        ~cont:(fun ~typed_recv ~typed_args ~ret_typ ~label ->
          Ast.replace_expr
            ~new_expr:
              (StaticDispatch
                 {
                   stat with
                   stat_recv = typed_recv;
                   stat_args = typed_args;
                   stat_label = Some label;
                 })
            expr
          |> Ast.add_type ~typ:ret_typ)

and aux_dispatch_common ~ctx ~expr ~method_id ~recv ~args ~recv_type_translator
    ~cont =
  let typed_recv = aux ~ctx recv in
  let typed_args = List.map ~f:(aux ~ctx) args in
  let all_types_exist =
    Option.is_some typed_recv.expr_typ
    && List.for_all ~f:(fun arg -> Option.is_some arg.expr_typ) typed_args
  in
  match all_types_exist with
  | false -> expr
  | true -> (
      let recv_typ = Option.get typed_recv.expr_typ in
      match recv_type_translator ~recv_typ with
      | None -> expr
      | Some trans_recv_typ -> (
          let method_sig_opt =
            Methodtbl.find_opt ctx.sigs ~inherit_tree:ctx.inherit_tree
              ~typ:trans_recv_typ ~method_id
          in
          match method_sig_opt with
          | None ->
              Semantprint.print_location expr.expr_loc;
              Printf.eprintf "Dispatch to undefined method %a.\n" T.print_id
                method_id;
              expr
          | Some method_sig -> (
              match List.compare_lengths typed_args method_sig.formals with
              | 0 -> (
                  let formal_ret_typ = method_sig.ret_typ in
                  let ret_typ =
                    if formal_ret_typ = T.self_type then recv_typ
                    else formal_ret_typ
                  in
                  let valid_arg_types =
                    List.for_all2
                      ~f:(validate_arg_types ~ctx ~loc:expr.expr_loc ~method_id)
                      typed_args method_sig.formals
                  in
                  match valid_arg_types with
                  | false -> expr
                  | true ->
                      cont ~typed_recv ~typed_args ~ret_typ
                        ~label:method_sig.label)
              | _ ->
                  Semantprint.print_location expr.expr_loc;
                  Printf.eprintf
                    "Method %a called with wrong number of arguments.\n"
                    T.print_id method_id;
                  expr)))

and validate_arg_types ~ctx ~loc ~method_id arg (id, formal_typ) =
  let arg_typ = Option.get arg.expr_typ in
  match
    Types.is_subtype ctx.inherit_tree ~cl_typ:ctx.cl_typ ~sub_typ:arg_typ
      ~super_typ:formal_typ
  with
  | true -> true
  | false ->
      Semantprint.print_location loc;
      Printf.eprintf
        "In call of method %a, type %a of parameter %a does not conform to \
         declared type %a.\n"
        T.print_id method_id T.print_type arg_typ T.print_id id T.print_type
        formal_typ;
      false

and aux_case ~ctx ~expr ~case_expr =
  let typed_case_expr = aux ~ctx case_expr.case_expr in
  let branches = case_expr.case_branches in
  let typed_branches_opt =
    List.rev_map ~f:(aux_branch ~ctx) branches |> Optutil.flatten_opt_list
  in
  let typ_tbl = Hashtbl.create ((List.length branches * 2) - 1) in
  let branches_unique =
    List.fold_left ~f:(dedup_branches ~typ_tbl) ~init:true branches
  in
  let is_valid = branches_unique && Option.is_some typed_case_expr.expr_typ in
  match (is_valid, typed_branches_opt) with
  | false, _ -> expr
  | true, None -> expr
  | true, Some typed_branches ->
      let branch_types =
        List.map
          ~f:(fun branch ->
            let _, typed_body = branch.elem in
            Option.get typed_body.expr_typ)
          typed_branches
      in
      Ast.replace_expr
        ~new_expr:
          (Case { case_expr = typed_case_expr; case_branches = typed_branches })
        expr
      |> Ast.add_type
           ~typ:
             (all_lca ~inherit_tree:ctx.inherit_tree ~cl_typ:ctx.cl_typ
                ~typs:branch_types)

and dedup_branches ~typ_tbl acc branch =
  match acc with
  | false -> false
  | true -> (
      let (_, var_typ), _ = branch.elem in
      match Hashtbl.mem typ_tbl var_typ with
      | false ->
          Hashtbl.replace typ_tbl ~key:var_typ ~data:();
          true
      | true ->
          Semantprint.print_location branch.loc;
          Printf.eprintf "Duplicate branch %a in case statement.\n" T.print_type
            var_typ;
          false)

and aux_branch ~ctx branch =
  let (id, var_typ), body = branch.elem in
  let is_valid_id = validate_branch_id ~loc:branch.loc ~id in
  let is_valid_typ = validate_branch_typ ~ctx ~loc:branch.loc ~id ~var_typ in
  match is_valid_id && is_valid_typ with
  | false -> None
  | true ->
      Symtbl.enter_scope ctx.id_env
        ~cont:
          (lazy
            (Symtbl.add ctx.id_env ~key:id ~data:var_typ |> ignore;
             let typed_body = aux ~ctx body in
             match typed_body.expr_typ with
             | None -> None
             | Some _ -> Some { branch with elem = ((id, var_typ), typed_body) }))

and validate_branch_id ~loc ~id =
  match id <> T.self_var with
  | true -> true
  | false ->
      Semantprint.print_location loc;
      prerr_endline "'self' bound in 'case'.";
      false

and validate_branch_typ ~ctx ~loc ~id ~var_typ =
  match var_typ <> T.self_type with
  | false ->
      Semantprint.print_location loc;
      Printf.eprintf
        "Identifier %a declared with type SELF_TYPE in case branch.\n"
        T.print_id id;
      false
  | true -> (
      match Tree.mem ctx.inherit_tree var_typ with
      | true -> true
      | false ->
          Semantprint.print_location loc;
          Printf.eprintf "Class %a of case branch is undefined.\n" T.print_type
            var_typ;
          false)

and rec_helper ~ctx ~cont ~err_fun ~expr ~sub_expr ~super_typ =
  let typed_sub_expr = aux ~ctx sub_expr in
  match typed_sub_expr.expr_typ with
  | None -> sub_expr
  | Some sub_expr_typ -> (
      match
        Types.is_subtype ctx.inherit_tree ~cl_typ:ctx.cl_typ ~sub_typ:sub_expr_typ
          ~super_typ
      with
      | true -> cont ~typed_sub_expr sub_expr_typ
      | false ->
          err_fun ~sub_expr_typ;
          expr)

let typecheck ~ctx ~expr = aux ~ctx expr
