(* intermediaterepr.ml *)

open Util

type temp = int

type var = Tables.id_sym * int

type lhs =
  | ITemp of temp
  | IVar of var
  | RetReg
  | SelfReg

type rhs =
  | IRef of lhs
  | IInt32Const of int32
  | IStrConst of Tables.str_sym
  | IAdd of lhs * lhs
  | ISub of lhs * lhs
  | IMult of lhs * lhs
  | IDiv of lhs * lhs
  | INeg of lhs
  | ILt of lhs * lhs
  | ILe of lhs * lhs
  | IEq of lhs * lhs
  | INot of lhs

type block_id = int
type label = Tables.str_sym

type stmt =
  | IAssign of lhs * rhs
  | Jump of block_id
  | BrEq of lhs * lhs * block_id
  | BrNotEq of lhs * lhs * block_id
  | BrLt of lhs * lhs * block_id
  | BrLe of lhs * lhs * block_id
  | BrGt of lhs * lhs * block_id
  | BrGe of lhs * lhs * block_id
  | BrZero of lhs * block_id
  | BrNonZero of lhs * block_id
  | Call of label
  | IndirectCall of lhs

type basic_block = stmt list
