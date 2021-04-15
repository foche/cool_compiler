(* intermediaterepr.ml *)

open Util

type temp = int

type var = Tables.id_sym * int

type reg = ITemp of temp | IVar of var | RetReg | SelfReg

type const = IInt32Const of int32 | IStrConst of Tables.str_sym

type arith = IAdd | ISub | IMul | IDiv

type comp = ILt | ILe | IGt | IGe | IEq | INotEq

type binop = IArith of arith | IComp of comp

type unary = INeg | INot

type value =
  | IReg of reg
  | IConst of const
  | IArith of arith * reg * reg
  | IArithImm of arith * reg * const
  | IComp of comp * reg * reg
  | ICompImm of comp * reg * const
  | IUnary of unary * reg

type block_id = int

type label = Tables.id_sym

type stmt =
  | IAssign of reg * value
  | Jump of block_id
  | Branch of comp * reg * reg * block_id
  | BrZero of reg * block_id
  | BrNonZero of reg * block_id
  | Call of label * reg list
  | IndirectCall of reg * reg list
  | Return

type basic_block = stmt list
