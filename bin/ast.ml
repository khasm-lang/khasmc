type loc = Lexing.position
type binop =
  | BinOpPlus
  | BinOpMinus
  | BinOpMul

type unop =
  | UnOpRef
  | UnOpDeref
  | UnOpPos
  | UnOpNeg

type typeSig =
  | Base of string
  | Arrow of typeSig * typeSig

type const =
  | Int of string
  | Float of string
  | String of string
  | Ident of string

type expr =
  | Base of const
  | UnOp of unop * expr
  | BinOp of expr * binop * expr
  | FuncCall of expr * expr

type if_t =
  Cond of expr * Body of block

type while_t =
  Cond of expr * Body of block


type block =
  | Many of block list
  | AssignBlock of Ident * block
  | Assign of Ident * expr
  | Typesig of Ident * typeSig
  | If of if_t
  | While of while_t


type toplevel =
  | AssignBlock of Ident * block
  | Assign of Ident * expr
  | Typesig of Ident * typeSig

type program = Prog of toplevel list
