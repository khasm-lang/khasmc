type ident = string

type binop =
  | BinOpPlus
  | BinOpMinus
  | BinOpMul
  | BinOpDiv

type unop =
  | Many of unop list
  | UnOpRef
  | UnOpDeref
  | UnOpPos
  | UnOpNeg

type typeSig =
  | Ptr of int * string
  | Base of string
  | Arrow of typeSig * typeSig

type const =
  | Int of string
  | Float of string
  | String of string
  | Id of ident
  | True
  | False

type expr =
  | Paren of expr
  | Base of const
  | UnOp of unop list * expr
  | BinOp of expr * binop * expr
  | FuncCall of expr * expr list


type block =
  | Many of block list
  | AssignBlock of ident * block
  | Assign of ident * expr
  | Typesig of ident * typeSig
  | If of if_t
  | While of while_t
  | Return of expr

and if_t = expr * block

and while_t = expr * block


type toplevel =
  | AssignBlock of ident * block
  | Assign of ident * expr
  | Typesig of ident * typeSig

type program = Prog of toplevel list
(* Local Variables: *)
(* caml-annot-dir: "_build/default/bin/.main.eobjs/byte" *)
(* End: *)
