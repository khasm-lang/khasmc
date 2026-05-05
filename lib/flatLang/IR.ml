open Share.Uuid
open Share.Maybe

type name = Parsing.Ast.resolved
[@@deriving show { with_path = false }]

module Name = struct
  type t = name
  let compare = compare
end

module NameMap = Map.Make(Name)

(* we need to reuse the same counter to ensure
   we don't generate conflicting names
   *)
let fresh_name () = Parsing.Ast.fresh_resolved ()

(* No more polymorphism. *)
type typ =
  (* for type unconstrained at any point; can be
     reduced to a ZST
     TODO: look into instance merging?
  *)
  | TyUnknown
  | TyBase of
      [ `Irr | `Bottom | `Int | `String | `Char | `Float | `Bool ]
  | TyTuple of typ list
  | TyArrow of typ * typ
  | TyCustom of name * typ list
  | TyRef of typ
[@@deriving show { with_path = false }]

type binop = Parsing.Ast.binop [@@deriving show { with_path = false }]

type unaryop =
  | Negate
  | BNegate
  | Ref
  | Project of int
[@@deriving show { with_path = false }]

(* Type param is "has lambdas "*)

type tag =
  | Fail of string
  | Named of [ `Local | `Global | `Constructor of typ list ] * name
  | Prim of [ `Int | `String | `Char | `Float ] * string
  | Bool of bool
  | Tuple
  | BinOp of binop
  | UnaryOp of unaryop
  | Lambda of name * typ (* input type *)
  | Funccall
  | Unpack of typ list * name list
  | Let of name (* binding name *)
  | IfLet of name (* ctor name *)
  | Seq
  | Modify of name
[@@deriving show { with_path = false }]

type data = {
  uuid : unit uuid;
  mutable typ : typ;
}
[@@deriving show { with_path = false }]

type expr = Expr of (data[@opaque]) * tag * expr list
[@@deriving show { with_path = false }]

let get_typ (Expr (dat, _, _)) = dat.typ
let get_children (Expr (_, _, children)) = children

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
  constructors : constructor list;
}
[@@deriving show { with_path = false }]

let process_in_definitions func prog =
  let fold_def def = { def with body = func def.body } in
  { prog with defs = List.map fold_def prog.defs }

let process_in_definitions' func prog =
  let fold_def def = { def with body = func def def.body } in
  { prog with defs = List.map fold_def prog.defs }
