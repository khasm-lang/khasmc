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

let rec typeSigEq t1 t2 =
  match t1, t2 with
  | Ptr (i1, s1), Ptr (i2, s2) ->
     (i1 = i2) && (s1 = s2)
  | Base (b1), Base (b2) -> b1 = b2
  | Arrow (t11, t12), Arrow (t21, t22) ->
     typeSigEq t11 t21 && typeSigEq t12 t22
  | _, _ -> false

let typeSigLeft ts : typeSig option =
  match ts with
  | Ptr (_, _) -> None
  | Base (_) -> None
  | Arrow (x, _) -> Some(x)


let typeSigRight ts : typeSig option =
  match ts with
  | Ptr (_, _) -> None
  | Base (_) -> None
  | Arrow (_, y) -> Some(y)

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
  | AssignBlock of ident * ident list * block
  | Assign of ident * ident list * expr
  | Typesig of ident * typeSig
  | If of if_t
  | While of while_t
  | Return of expr

and if_t = expr * block

and while_t = expr * block


type toplevel =
  | AssignBlock of ident * ident list * block
  | Assign of ident * ident list * expr
  | Typesig of ident * typeSig

type program = Prog of toplevel list
(* Local Variables: *)
(* caml-annot-dir: "_build/default/bin/.main.eobjs/byte" *)
(* End: *)
