(* exprchecker.ml *)

open Parser
open Ast
open Util
open Helpers
open Tables

type context = {
    id_env : (id_sym, type_sym) Typeenv.t;
    sigs : Methodtbl.t;
    graph : type_sym Tree.t;
    cl : clazz;
    filename : str_sym;
  }

let lca graph cl typ1 typ2 =
  Tree.lca graph (translate_type cl typ1) (translate_type cl typ2)

let all_lca graph cl typs =
  List.map (translate_type cl) typs |> Tree.all_lca graph

let validate_let_var_not_self filename line_num id =
  match id != self_var with
  | true -> true
  | false ->
    Semantprint.print_location filename line_num;
    prerr_endline "'self' cannot be bound in a 'let' expression.";
    false

let validate_let_type filename line_num graph id typ =
  match Tree.mem graph typ with
  | true -> true
  | false ->
    Semantprint.print_location filename line_num;
    Printf.eprintf
      "Class %a of let-bound identifier %a is undefined.\n"
      print_type typ
      print_id id;
    false

let validate_arg_types_match ctx method_id (arg, line_num) (id, formal_typ) =
  let arg_typ = get_opt arg.typ_type in
  match is_subtype ctx.graph ctx.cl arg_typ formal_typ with
  | true -> true
  | false ->
    Semantprint.print_location ctx.filename line_num;
    Printf.eprintf
      "In call of method %a, type %a of parameter %a does not conform to declared type %a.\n"
      print_id method_id
      print_type arg_typ
      print_id id
      print_type formal_typ;
    false

let rec aux ~ctx (exp, _ as exp_node) =
  match exp.typ_expr with
  | IntConst _ -> add_type exp_node int_type
  | StrConst _ -> add_type exp_node string_type
  | BoolConst _ -> add_type exp_node bool_type
  | Variable id -> aux_var ~ctx exp_node id
  | Assign (id, exp_node') -> aux_assign ~ctx exp_node id exp_node'
  | New typ -> aux_new ~ctx exp_node typ
  | Cond (if_exp_node, then_exp_node, else_exp_node) ->
    aux_cond ~ctx exp_node if_exp_node then_exp_node else_exp_node
  | Block exp_nodes -> aux_block ~ctx exp_node exp_nodes
  | IsVoid exp_node' -> aux_isvoid ~ctx exp_node exp_node'
  | Loop (pred_exp_node, body_exp_node) -> aux_loop ~ctx exp_node pred_exp_node body_exp_node
  | Not exp_node' -> aux_not ~ctx exp_node exp_node'
  | Neg exp_node' -> aux_neg ~ctx exp_node exp_node'
  | Add (e1, e2) -> aux_binop ~ctx exp_node "+" (fun e1' e2' -> Add (e1', e2')) e1 e2 int_type
  | Sub (e1, e2) -> aux_binop ~ctx exp_node "-" (fun e1' e2' -> Sub (e1', e2')) e1 e2 int_type
  | Mult (e1, e2) -> aux_binop ~ctx exp_node "*" (fun e1' e2' -> Mult (e1', e2')) e1 e2 int_type
  | Div (e1, e2) -> aux_binop ~ctx exp_node "/" (fun e1' e2' -> Div (e1', e2')) e1 e2 int_type
  | Lt (e1, e2) -> aux_binop ~ctx exp_node "<" (fun e1' e2' -> Lt (e1', e2')) e1 e2 bool_type
  | Le (e1, e2) -> aux_binop ~ctx exp_node "<=" (fun e1' e2' -> Le (e1', e2')) e1 e2 bool_type
  | Eq (e1, e2) -> aux_eq ~ctx exp_node e1 e2
  | Let let_stmt -> aux_let ~ctx exp_node let_stmt
  | DynDispatch dyn -> aux_dyn_dispatch ~ctx exp_node dyn
  | StaticDispatch stat -> aux_stat_dispatch ~ctx exp_node stat
  | Case (exp_node', branches) -> aux_case ~ctx exp_node exp_node' branches
  | NoExpr -> exp_node

and aux_var ~ctx (_, line_num as exp_node) id =
  match Typeenv.find_opt ctx.id_env id with
  | Some typ -> add_type exp_node typ
  | None ->
    Semantprint.print_location ctx.filename line_num;
    Printf.eprintf
      "Undeclared identifier %a.\n"
      print_id id;
    exp_node

and aux_assign ~ctx (_, line_num as exp_node) id exp_node' =
  match id = self_var with
  | true ->
    Semantprint.print_location ctx.filename line_num;
    prerr_endline "Cannot assign to 'self'.\n";
    exp_node
  | false ->
    match Typeenv.find_opt ctx.id_env id with
    | None ->
      Semantprint.print_location ctx.filename line_num;
      Printf.eprintf
        "Assignment to undeclared variable %a.\n"
        print_id id;
      exp_node
    | Some typ ->
      rec_helper
        ~ctx
        ~cont:(fun typed_exp_node' typ' -> {
            typ_expr = Assign (id, typed_exp_node');
            typ_type = Some typ';
          },
          line_num)
        ~err_fun:(fun typ' ->
          Semantprint.print_location ctx.filename line_num;
          Printf.eprintf
            "Type %a of assigned expression does not conform to declared type %a of identifier %a.\n"
            print_type typ'
            print_type typ
            print_id id)
        (exp_node', typ)

and aux_new ~ctx (_, line_num as exp_node) typ =
  match Tree.mem ctx.graph typ with
  | true -> add_type exp_node typ
  | false ->
    Semantprint.print_location ctx.filename line_num;
    Printf.eprintf
      "'new' used with undefined class %a.\n"
      print_type typ;
    exp_node

and aux_cond ~ctx (_, line_num as exp_node) if_exp_node then_exp_node else_exp_node =
  rec_helper
    ~ctx
    ~cont:(fun if_exp_node' _ ->
      let then_exp_node' = aux ~ctx then_exp_node in
      let else_exp_node' = aux ~ctx else_exp_node in
      match get_exp_type then_exp_node', get_exp_type else_exp_node' with
      | Some then_typ, Some else_typ -> {
          typ_expr = Cond (if_exp_node', then_exp_node', else_exp_node');
          typ_type = Some (lca ctx.graph ctx.cl then_typ else_typ);
        },
        line_num
      | _ -> exp_node)
    ~err_fun:(fun _ ->
      Semantprint.print_location ctx.filename line_num;
      prerr_endline "Predicate of 'if' does not have type Bool.")
    (if_exp_node, bool_type)

and aux_block ~ctx (_, line_num as exp_node) exp_nodes =
  let exp_nodes' = List.rev_map (aux ~ctx) exp_nodes in
  match List.for_all (fun (exp', _) -> is_some_opt exp'.typ_type) exp_nodes' with
  | true -> {
      typ_expr = Block (List.rev exp_nodes');
      typ_type = List.hd exp_nodes' |> get_exp_type;
    },
    line_num
  | false -> exp_node

and aux_isvoid ~ctx (_, line_num as exp_node) exp_node' =
  let typed_exp_node' = aux ~ctx exp_node' in
  match get_exp_type typed_exp_node' with
  | None -> exp_node
  | Some _ -> {
      typ_expr = IsVoid typed_exp_node';
      typ_type = Some bool_type;
    },
    line_num

and aux_loop ~ctx (_, line_num as exp_node) pred_exp_node body_exp_node =
  rec_helper
    ~ctx
    ~cont:(fun pred_exp_node' _ ->
      let body_exp_node' = aux ~ctx body_exp_node in
      match get_exp_type body_exp_node' with
      | None -> exp_node
      | Some _ -> {
          typ_expr = Loop (pred_exp_node', body_exp_node');
          typ_type = Some object_type;
        },
        line_num)
    ~err_fun:(fun _ ->
      Semantprint.print_location ctx.filename line_num;
      prerr_endline "Loop condition does not have type Bool.")
    (pred_exp_node, bool_type)

and aux_not ~ctx (_, line_num) exp_node' =
  rec_helper
    ~ctx
    ~cont:(fun typed_node _ -> {
        typ_expr = Not typed_node;
        typ_type = Some bool_type;
      },
      line_num)
    ~err_fun:(fun typ' ->
      Semantprint.print_location ctx.filename line_num;
      Printf.eprintf
        "Argument of 'not' has type %a instead of Bool.\n"
        print_type typ')
    (exp_node', bool_type)

and aux_neg ~ctx (_, line_num) exp_node' =
  rec_helper
    ~ctx
    ~cont:(fun typed_node _ -> {
        typ_expr = Neg typed_node;
        typ_type = Some int_type;
      },
      line_num)
    ~err_fun:(fun typ' ->
      Semantprint.print_location ctx.filename line_num;
      Printf.eprintf
        "Argument of '~' has type %a instead of Bool.\n"
        print_type typ')
    (exp_node', int_type)

and aux_binop ~ctx (_, line_num as exp_node) op f e1 e2 exp_typ =
  let exp_nodes' = Array.map (aux ~ctx) [| e1; e2 |] in
  match get_exp_type exp_nodes'.(0), get_exp_type exp_nodes'.(1) with
  | Some typ1, Some typ2 ->
    (match typ1 = int_type && typ2 = int_type with
    | true -> {
        typ_expr = f exp_nodes'.(0) exp_nodes'.(1);
        typ_type = Some exp_typ;
      },
      line_num
    | false ->
      Semantprint.print_location ctx.filename line_num;
      Printf.eprintf
        "non-Int arguments: %a %s %a\n"
        print_type typ1
        op
        print_type typ2;
      exp_node)
  | _ -> exp_node

and aux_eq ~ctx (_, line_num as exp_node) e1 e2 =
  let exp_nodes' = Array.map (aux ~ctx) [| e1; e2 |] in
  match get_exp_type exp_nodes'.(0), get_exp_type exp_nodes'.(1) with
  | Some typ1, Some typ2 ->
    let primitives = [int_type; string_type; bool_type] in
    let typ1_primitive = List.mem typ1 primitives in
    let typ2_primitive = List.mem typ2 primitives in
    (match (not typ1_primitive && not typ2_primitive) || typ1 = typ2 with
    | true -> {
        typ_expr = Eq (exp_nodes'.(0), exp_nodes'.(1));
        typ_type = Some bool_type;
      },
      line_num
    | false ->
      Semantprint.print_location ctx.filename line_num;
      prerr_endline "Illegal comparison with a basic type.";
      exp_node)
  | _ -> exp_node

and aux_let ~ctx (_, line_num as exp_node) let_stmt =
  let id = let_stmt.let_var in
  let typ = let_stmt.let_var_type in
  let init = let_stmt.let_init in
  let is_valid_var = validate_let_var_not_self ctx.filename line_num id in
  let is_valid_type = validate_let_type ctx.filename line_num ctx.graph id typ in
  match is_valid_var && is_valid_type, (fst init).typ_expr with
  | false, _ -> exp_node
  | true, NoExpr -> aux_let_helper ~ctx exp_node let_stmt init
  | true, _ ->
    rec_helper
      ~ctx
      ~cont:(fun typed_init _ -> aux_let_helper ~ctx exp_node let_stmt typed_init)
      ~err_fun:(fun typ' ->
        Semantprint.print_location ctx.filename line_num;
        Printf.eprintf
          "Inferred type %a of initialization of %a does not conform to identifier's declared type %a.\n"
          print_type typ'
          print_id id
          print_type typ)
      (init, typ)

and aux_let_helper ~ctx (_, line_num as exp_node) let_stmt typed_init =
  Typeenv.enter_scope ctx.id_env (lazy (
    Typeenv.add ctx.id_env let_stmt.let_var let_stmt.let_var_type |> ignore;
    let typed_body = aux ~ctx let_stmt.let_body in
    match get_exp_type typed_body with
    | None -> exp_node
    | Some body_typ -> {
        typ_expr = Let {
          let_stmt with
          let_init = typed_init;
          let_body = typed_body;
        };
        typ_type = Some body_typ;
      },
      line_num))

and aux_dyn_dispatch ~ctx (_, line_num as exp_node) dyn =
  aux_dispatch_common
    ~ctx
    ~exp_node
    ~method_id:dyn.dyn_method
    ~recv:dyn.dyn_recv
    ~args:dyn.dyn_args
    ~validate_recv_type:(fun recv_typ -> Some (translate_type ctx.cl recv_typ))
    ~cont:(fun recv args ret_type -> {
        typ_expr = DynDispatch {
          dyn with
          dyn_recv = recv;
          dyn_args = args;
        };
        typ_type = Some ret_type;
      },
      line_num)

and aux_stat_dispatch ~ctx (_, line_num as exp_node) stat =
  match Tree.mem ctx.graph stat.stat_type with
  | false ->
    Semantprint.print_location ctx.filename line_num;
    Printf.eprintf
      "Static dispatch to undefined class %a.\n"
      print_type stat.stat_type;
    exp_node
  | true ->
    aux_dispatch_common
      ~ctx
      ~exp_node
      ~method_id:stat.stat_method
      ~recv:stat.stat_recv
      ~args:stat.stat_args
      ~validate_recv_type:(fun recv_typ ->
        match is_subtype ctx.graph ctx.cl recv_typ stat.stat_type with
        | true -> Some stat.stat_type
        | false ->
          Semantprint.print_location ctx.filename line_num;
          Printf.eprintf
            "Expression type %a does not conform to declared static dispatch type %a.\n"
            print_type recv_typ
            print_type stat.stat_type;
          None)
      ~cont:(fun recv args ret_type -> {
          typ_expr = StaticDispatch {
            stat with
            stat_recv = recv;
            stat_args = args;
          };
          typ_type = Some ret_type;
        },
        line_num)

and aux_dispatch_common ~ctx ~exp_node ~method_id ~recv ~args ~validate_recv_type ~cont =
  let recv' = aux ~ctx recv in
  let args' = List.map (aux ~ctx) args in
  let arg_types_exist = List.for_all (fun arg -> get_exp_type arg |> is_some_opt) args' in
  let all_types_exist = arg_types_exist && get_exp_type recv' |> is_some_opt in
  match all_types_exist with
  | false -> exp_node
  | true ->
    let recv_type = get_opt (get_exp_type recv') in
    match validate_recv_type recv_type with
    | None -> exp_node
    | Some recv_type' ->
      let method_sig_opt =
        Methodtbl.find_opt
          ~tbl:ctx.sigs
          ~graph:ctx.graph
          ~clazz:recv_type'
          ~method_id in
      match method_sig_opt with
      | None ->
        Semantprint.print_location ctx.filename (snd exp_node);
        Printf.eprintf
          "Dispatch to undefined method %a.\n"
          print_id method_id;
        exp_node
      | Some method_sig ->
        let ret_type = method_sig.return_type in
        let ret_type' = if ret_type = self_type then recv_type else ret_type in
        match List.compare_lengths args' method_sig.formals with
        | 0 ->
          let valid_arg_types =
            List.for_all2
              (validate_arg_types_match ctx method_id)
              args'
              method_sig.formals in
          (match valid_arg_types with
          | false -> exp_node
          | true -> cont recv' args' ret_type')
        | _ ->
          Semantprint.print_location ctx.filename (snd exp_node);
          Printf.eprintf
            "Method %a called with wrong number of arguments.\n"
            print_id method_id;
          exp_node

and aux_case ~ctx (_, line_num as exp_node) exp_node' branches =
  let typed_exp_node' = aux ~ctx exp_node' in
  let typed_branches_opt = List.rev_map (aux_branch ~ctx) branches |> flatten_opt_list in
  let typ_tbl = Hashtbl.create 16 in
  let branches_unique = List.fold_left (dedup_branches typ_tbl ctx.filename) true branches in
  let is_valid =
    branches_unique &&
    get_exp_type typed_exp_node' |> is_some_opt &&
    is_some_opt typed_branches_opt in
  match is_valid with
  | false -> exp_node
  | true ->
    let typed_branches = get_opt typed_branches_opt in
    let branch_types =
      List.map (fun ((_, _, typed_body), _) -> get_opt (get_exp_type typed_body)) typed_branches in
    {
      typ_expr = Case (typed_exp_node', typed_branches);
      typ_type = Some (all_lca ctx.graph ctx.cl branch_types);
    },
    line_num

and dedup_branches typ_tbl filename acc ((_, typ, _), line_num) =
  match acc with
  | false -> false
  | true ->
    match Hashtbl.mem typ_tbl typ with
    | false -> Hashtbl.replace typ_tbl typ (); true
    | true ->
      Semantprint.print_location filename line_num;
      Printf.eprintf
        "Duplicate branch %a in case statement.\n"
        print_type typ;
      false

and aux_branch ~ctx ((id, typ, body), line_num) =
  let is_valid_id = validate_branch_id ~ctx line_num id in
  let is_valid_typ = validate_branch_typ ~ctx line_num id typ in
  match is_valid_id && is_valid_typ with
  | false -> None
  | true ->
    Typeenv.enter_scope ctx.id_env (lazy (
      Typeenv.add ctx.id_env id typ |> ignore;
      let typed_body = aux ~ctx body in
      match get_exp_type typed_body with
      | None -> None
      | Some _ -> Some ((id, typ, typed_body), line_num)))

and validate_branch_id ~ctx line_num id =
  match id != self_var with
  | true -> true
  | false ->
    Semantprint.print_location ctx.filename line_num;
    prerr_endline "'self' bound in 'case'.";
    false

and validate_branch_typ ~ctx line_num id typ =
  match typ != self_type with
  | false ->
    Semantprint.print_location ctx.filename line_num;
    Printf.eprintf
      "Identifier %a declared with type SELF_TYPE in case branch.\n"
      print_id id;
    false
  | true ->
    match Tree.mem ctx.graph typ with
    | true -> true
    | false ->
      Semantprint.print_location ctx.filename line_num;
      Printf.eprintf
        "Class %a of case branch is undefined.\n"
        print_type typ;
      false

and rec_helper ~ctx ~cont ~err_fun (exp_node, expected_typ) =
  let typed_exp_node = aux ~ctx exp_node in
  match get_exp_type typed_exp_node with
  | None -> exp_node
  | Some typ ->
    match is_subtype ctx.graph ctx.cl typ expected_typ with
    | false ->
      err_fun typ;
      exp_node
    | true -> cont typed_exp_node typ

let typecheck ~ctx ~exp_node =
  aux ~ctx exp_node
