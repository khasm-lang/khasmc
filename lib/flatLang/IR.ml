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
  | TyUnknown
  | TyBase of [ `Irr
              | `Bottom
              | `Int
              | `String
              | `Char
              | `Float
              | `Bool
              ]
  | TyTuple of typ list
  | TyArrow of typ * typ
  | TyCustom of name * typ list
  | TyRef of typ
[@@deriving show { with_path = false }]

type binop = Parsing.Ast.binop [@@deriving show { with_path = false }]

type unaryop = name Parsing.Ast.unaryop
[@@deriving show { with_path = false }]

(* Type param is "has lambdas "*)

type tag =
  | Fail of string
  | Named of [ `Local | `Global | `Constructor ] * name
  | Prim of [ `Int | `String | `Char | `Float ] * string
  | Bool of bool
  | Tuple
  | BinOp of binop
  | UnaryOp of unaryop
  | Lambda of name
  | Funccall
  | Record of name * name list
  | ConstructorField of int * typ list
  | Let of name (* binding name *)
  | IfLet of name (* ctor name *)
  | Seq
  | Modify of name
[@@deriving show { with_path = false }]

type data = {
  uuid: unit uuid; [@opaque]
  mutable typ: typ;
}
[@@deriving show { with_path = false }]

type expr = Expr of data * tag * expr list
[@@deriving show { with_path = false }]

type record = {
  name : name;
  fields : name list;
}
[@@deriving show { with_path = false }]

module Ctor = struct

type constructor = {
  name : name;
  index : int; (* index in the type (for tag) *)
}
[@@deriving show { with_path = false }]

end
open Ctor

type definition = {
  name : name;
  args : (name * typ) list;
  returns : typ;
  body : expr;
}
[@@deriving show { with_path = false }]

type program = {
  defs : definition list;
  records : record list;
  constructors : constructor list;
}
[@@deriving show { with_path = false }]
