open Share.Uuid
open Share.Maybe

type name = Parsing.Ast.resolved
[@@deriving show { with_path = false }]

(* No more polymorphism. *)
type typ =
  (* for type unconstrained at any point; can be
     reduced to a ZST
     TODO: look into instance merging?
  *)
  | TyIrrelevant
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
[@@deriving show { with_path = false }]

type data = (name Parsing.Ast.typ Parsing.Ast.data[@opaque])
[@@deriving show { with_path = false }]

type binop = Parsing.Ast.binop [@@deriving show { with_path = false }]

type unaryop = name Parsing.Ast.unaryop
[@@deriving show { with_path = false }]

(* Type param is "has lambdas "*)
type expr =
  | Fail of data * string
  | Local of data * name
  | Global of data * name
  | Constructor of data * name
  | Int of data * string
  | String of data * string
  | Char of data * string
  | Float of data * string
  | Bool of data * bool
  | Tuple of data * expr list
  | BinOp of data * binop * expr * expr
  | UnaryOp of data * unaryop * expr
  (* The interesting case. *)
  | Lambda of data * name * expr
  | Funccall of data * expr * expr list
  | Record of data * name * (name * expr) list
  | Let of data * name * expr * expr
  | IfLet of data * name * expr * expr * expr
  | If of data * name * expr * expr * expr
  | Seq of data * expr * expr
  | Modify of data * name * expr
[@@deriving show { with_path = false }]

type record = {
  name : name;
  constructs : typ;
  fields : (name * typ) list;
}
[@@deriving show { with_path = false }]

type constructor = {
  name : name;
  constructs : typ;
  fields : (name * typ) list;
}
[@@deriving show { with_path = false }]

type 'a definition = {
  name : name;
  args : (name * typ) list;
  body : expr;
  has_lambdas : (unit, 'a) Share.Maybe.maybe; [@opaque]
}
[@@deriving show { with_path = false }]

type 'a program = {
  defs : 'a definition list;
  records : record list;
  constructors : constructor list;
}
[@@deriving show { with_path = false }]
