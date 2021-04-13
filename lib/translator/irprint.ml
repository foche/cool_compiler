(* irprint.ml *)

open Intermediaterepr
open Util
open Tables

let print_reg out reg =
  match reg with
  | ITemp n -> Printf.fprintf out "$t%d" n
  | IVar (id, n) -> Printf.fprintf out "$%a_%d" print_id id n
  | RetReg -> Printf.fprintf out "$ret_reg"
  | SelfReg -> Printf.fprintf out "$self_reg"

let print_const out const =
  match const with
  | IInt32Const x -> Int32.to_int x |> Printf.fprintf out "%d"
  | IStrConst handle -> Tables.find_str handle |> Printf.fprintf out "ref %S"

let get_unary_op_str op =
  match op with
  | INeg -> "-"
  | INot -> "!"

let get_arith_op_str op =
  match op with
  | IAdd -> "+"
  | ISub -> "-"
  | IMul -> "*"
  | IDiv -> "/"

let get_comp_str op =
  match op with
  | ILt -> "<"
  | ILe -> "<="
  | IGt -> ">"
  | IGe -> ">="
  | IEq -> "="
  | INotEq -> "!="

let print_value out value =
  match value with
  | IReg reg -> print_reg out reg
  | IConst const -> print_const out const
  | IArith (op, r1, r2) ->
    Printf.fprintf out "%a %s %a" print_reg r1 (get_arith_op_str op) print_reg r2
  | IArithImm (op, r, c) ->
    Printf.fprintf out "%a %s %a" print_reg r (get_arith_op_str op) print_const c
  | IComp (op, r1, r2) ->
    Printf.fprintf out "%a %s %a" print_reg r1 (get_comp_str op) print_reg r2
  | ICompImm (op, r, c) ->
    Printf.fprintf out "%a %s %a" print_reg r (get_comp_str op) print_const c
  | IUnary (op, reg) ->
    Printf.fprintf out "%s%a" (get_unary_op_str op) print_reg reg

let get_br_comp_str comp_op =
  match comp_op with
  | ILt -> "brlt"
  | ILe -> "brle"
  | IGt -> "brgt"
  | IGe -> "brge"
  | IEq -> "breq"
  | INotEq -> "brne"

let print_stmt stmt =
  let rec print_args is_first args =
    match args with
    | [] -> ()
    | arg :: args' ->
      if not is_first then print_string ", ";
      print_reg stdout arg;
      print_args false args' in

  match stmt with
  | IAssign (dest, value) -> Printf.printf "%a <- %a\n" print_reg dest print_value value
  | Jump block_id -> Printf.printf "jump %d\n" block_id
  | Branch (comp_op, r1, r2, block_id) ->
    Printf.printf "%s %a, %a, %d" (get_br_comp_str comp_op) print_reg r1 print_reg r2 block_id
  | BrZero (reg, block_id) -> Printf.printf "brz %a, %d" print_reg reg block_id
  | BrNonZero (reg, block_id) -> Printf.printf "brnz %a, %d" print_reg reg block_id
  | Call (target, args) ->
    Printf.printf "call %a(" print_id target;
    print_args true args;
    print_endline ")"
  | IndirectCall (addr_reg, args) ->
    Printf.printf "icall *%a(" print_reg addr_reg;
    print_args true args;
    print_endline ")"
  | Return -> print_endline "ret"

let print_block _ block =
  List.iter print_stmt block;
  print_endline ""

let print_ir blocks =
  Hashtbl.iter print_block blocks
