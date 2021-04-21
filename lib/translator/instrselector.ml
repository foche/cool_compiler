(* translator.ml

open Parser
open Ast
open Util
open Tables
open Intermediaterepr
module Vv = Varversion

type trans_args = {
  block_id : int;
  var_versions : Vv.t;
  temp : temp;
  block : basic_block;
}

let translator_verbose = ref false

let zero = IInt32Const Int32.zero

let one = IInt32Const Int32.one

let zero_val = IConst zero

let one_val = IConst one

(* let id_to_block = Hashtbl.create 64 *)

let new_temp args =
  let fresh_temp = ITemp args.temp in
  ({ args with temp = args.temp + 1 }, fresh_temp)

let interpret_int ~default str_repr =
  let const = try Int32.of_string str_repr with Failure _ -> default in
  IConst (IInt32Const const)

let interpret_pos_int handle =
  find_int handle |> interpret_int ~default:Int32.max_int

let interpret_neg_int handle =
  "-" ^ find_int handle |> interpret_int ~default:Int32.min_int

let str_val handle = IConst (IStrConst handle)

let prepend_block args dest rhs =
  { args with block = IAssign (dest, rhs) :: args.block }

let make_var args id is_read =
  IVar (id, (if is_read then Vv.find else Vv.add) args.var_versions id)

let get_arith_op op =
  match op with Add -> IAdd | Sub -> ISub | Mul -> IMul | Div -> IDiv

let get_arith_f op x y = IArith (get_arith_op op, x, y)

let get_comp_op op = match op with Lt -> ILt | Le -> ILe

let get_comp_f op x y = IComp (get_comp_op op, x, y)

(* TODO *)
(* let rec create_block block_id var_versions dest node = *)

let rec trans_exp args dest parent ((exp, _) as exp_node) =
  let args', rhs =
    match exp.typ_expr with
    | Variable id -> trans_var args id
    | Assign (id, exp_node') -> trans_assign args id exp_node exp_node'
    | DynDispatch dyn -> trans_dyn args exp_node dyn
    | StaticDispatch stat -> trans_stat args exp_node stat
    | Cond (if_node, then_node, else_node) ->
        trans_cond args exp_node if_node then_node else_node
    | Loop (pred_node, body_node) ->
        trans_loop args exp_node pred_node body_node
    | Block exp_nodes -> trans_block args exp_node exp_nodes
    | Let let_stmt -> trans_let args exp_node let_stmt
    | Case (exp_node', branches) -> trans_case args exp_node exp_node' branches
    | New typ -> trans_new args typ
    | IsVoid exp_node' -> trans_isvoid args exp_node exp_node'
    | Arith (op, e1, e2) -> get_arith_f op |> trans_binop args exp_node e1 e2
    | Neg exp_node' -> trans_neg args exp_node exp_node'
    | Comp (op, e1, e2) -> get_comp_f op |> trans_binop args exp_node e1 e2
    | Eq (e1, e2) ->
        trans_binop args exp_node e1 e2 (fun x y -> IComp (IEq, x, y))
    | Not exp_node' -> trans_not args exp_node exp_node'
    | IntConst handle -> (args, interpret_pos_int handle)
    | StrConst handle -> (args, str_val handle)
    | BoolConst x -> (args, if x then one_val else zero_val)
    | NoExpr -> trans_default_init args parent
  in
  prepend_block args' dest rhs

and trans_var args id = (args, IReg (make_var args id true))

and trans_neg args parent ((exp, _) as exp_node) =
  match exp.typ_expr with
  | IntConst handle -> (args, interpret_neg_int handle)
  | _ ->
      let args', fresh_temp = assign_exp_to_temp args parent exp_node in
      (args', IUnary (INeg, fresh_temp))

and trans_assign args id parent exp_node =
  let var = make_var args id false in
  (trans_exp args var parent exp_node, IReg var)

and trans_not args parent exp_node =
  let args', fresh_temp = assign_exp_to_temp args parent exp_node in
  (args', IUnary (INot, fresh_temp))

(* TODO *)
and trans_dyn args parent dyn =
  let args', temps = trans_seq args parent dyn.dyn_args in
  let args'', recv = assign_exp_to_temp args' parent dyn.dyn_recv in
  (recv, temps) |> ignore;
  (args'', zero_val)

and trans_stat args parent stat =
  let args', temps = trans_seq args parent stat.stat_args in
  let args'', recv = assign_exp_to_temp args' parent stat.stat_recv in
  let args''' =
    {
      args'' with
      block =
        Call (Optutil.get stat.stat_label, recv :: Array.to_list temps)
        :: args''.block;
    }
  in
  (args''', IReg RetReg)

(* TODO *)
and trans_cond args parent if_node then_node else_node =
  if_node |> ignore;
  then_node |> ignore;
  else_node |> ignore;
  parent |> ignore;
  (args, zero_val)

(* TODO *)
and trans_loop args parent pred_node body_node =
  pred_node |> ignore;
  body_node |> ignore;
  parent |> ignore;
  (args, zero_val)

and trans_block args parent exp_nodes =
  let args', temps = trans_seq args parent exp_nodes in
  (args', IReg temps.(Array.length temps - 1))

(* TODO *)
and trans_let args parent let_stmt =
  (* let args', temp = assign_exp_to_temp args parent let_stmt.let_init *)
  let_stmt |> ignore;
  parent |> ignore;
  (args, zero_val)

(* TODO *)
and trans_case args parent exp_node branches =
  exp_node |> ignore;
  parent |> ignore;
  branches |> ignore;
  (args, zero_val)

(* TODO *)
and trans_new args typ =
  typ |> ignore;
  (args, zero_val)

and trans_isvoid args parent ((exp, _) as exp_node) =
  let args', temp = assign_exp_to_temp args parent exp_node in
  match Optutil.get exp.typ_type |> is_prim with
  | true -> (args', one_val)
  | false -> (args', ICompImm (IEq, temp, zero))

and trans_binop args parent e1 e2 op_fun =
  let args', temps = trans_seq args parent [ e1; e2 ] in
  (args', op_fun temps.(0) temps.(1))

and trans_seq args parent exp_nodes =
  let temps = Array.make (List.length exp_nodes) (ITemp 0) in
  let args', _ =
    List.fold_left
      (fun (args', i) exp_node ->
        let args'', temp = new_temp args' in
        temps.(i) <- temp;
        (trans_exp args'' temp parent exp_node, i + 1))
      (args, 0) exp_nodes
  in
  (args', temps)

and trans_default_init args (exp, _) =
  match Optutil.get exp.typ_type = string_type with
  | true -> (args, str_val empty_str)
  | false -> (args, zero_val)

and assign_temp_to_var args id temp =
  let var = make_var args id false in
  prepend_block args var temp

and assign_exp_to_temp args parent exp_node =
  let args', temp = new_temp args in
  (trans_exp args' temp parent exp_node, temp)

let translate_method blocks typ mthd =
  let args =
    { block_id = 0; var_versions = Vv.create 64; temp = 0; block = [] }
  in
  let args' = trans_exp args RetReg no_expr mthd.method_body in
  let args'' = { args' with block = Return :: args'.block } in
  List.rev args''.block |> Hashtbl.add blocks (typ, mthd.method_id)

let translate_feature blocks typ (feat, _) =
  match feat with
  | Method mthd -> translate_method blocks typ mthd
  | Field _ -> ()

let translate_class blocks (clazz, _) =
  List.iter (translate_feature blocks clazz.class_type) clazz.class_features

let translate_program (classes, _) =
  let blocks = Hashtbl.create 64 in
  List.iter (translate_class blocks) classes;
  blocks *)
