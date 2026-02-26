open Share.Uuid
open Share.Maybe

type name = Parsing.Ast.resolved

(* No more polymorphism. *)
type typ =
  | TyBottom
  | TyInt
  | TyString
  | TyChar
  | TyFloat
  | TyBool
  | TyTuple of typ list
  | TyArrow of typ * typ
  | TyCustom of name * typ list
  | TyRef of typ

type data = name Parsing.Ast.data

type binop = Parsing.Ast.binop

type unaryop = name Parsing.Ast.unaryop

(* Type param is "has lambdas "*)
type 'a expr =
  | Fail of data * string
  | Local of data * string
  | Global of data * string
  | Constructor of data * string
  | Int of data * string
  | Char of data * string
  | Float of data * string
  | Bool of data * bool
  | Tuple of data * 'a expr list
  | BinOp of data * binop * 'a expr * 'a expr
  | UnaryOp of data * unaryop * 'a expr
  (* The interesting case. *)
  | Lambda : data * name * 'a expr -> yes expr
  | Funccall of data * 'a expr * 'a expr list
  | Record of data * name * (name * 'a expr) list
  | Let of data * name * 'a expr * 'a expr
  | IfLet of data * name * 'a expr * 'a expr * 'a expr
  | If of data * name * 'a expr * 'a expr * 'a expr
  | Seq of data * 'a expr * 'a expr
  | Modify of data * name * 'a expr

type record = {
  name : name;
  constructs : typ;
  fields : (name * typ) list;
}

type constructor = {
  name : name;
  constructs : typ;
  fields : (name * typ) list;
}

type 'a definition = {
  name : name;
  args : name list;
  body : 'a expr;
}

type 'a program = {
  toplevel : 'a definition list;
  records  : record list;
  constructors : constructor list;
}
