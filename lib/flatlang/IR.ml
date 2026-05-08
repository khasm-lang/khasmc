open Share.Uuid
open Share.Maybe

(* Names are unique _within definitions_ but are not
   guaranteed to be unique across definitions
   *)
type name = Frontend.Ast.resolved
[@@deriving show { with_path = false }]

module Name = struct
  type t = name

  let compare = compare
end

module NameMap = Map.Make (Name)
module NameSet = Set.Make (Name)

(* we need to reuse the same counter to ensure
   we don't generate conflicting names
   *)
let fresh_name () = Frontend.Ast.fresh_resolved ()

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

type binop = Frontend.Ast.binop
[@@deriving show { with_path = false }]

type unaryop =
  | Negate
  | BNegate
  | Ref
  | Project of int
[@@deriving show { with_path = false }]

(* Type param is "has lambdas "*)

type const = [`Int | `String | `Char | `Float | `Bool] * string
[@@deriving show {with_path = false}]

type tag =
  | Fail of string
  (* doesn't neatly fit in Named *)
  | Extern of string
  | Named of [ `Local | `Global | `Constructor of typ list ] * name
  | Prim of const
  | Tuple
  | BinOp of binop
  | UnaryOp of unaryop
  | Lambda of name * typ (* input type *)
  | Funccall
  | Unpack of typ list * name list
  | Let of name (* binding name *)
  | IfLet of name (* ctor name *)
  | IfConst of const
  | Seq
  | Modify of name
[@@deriving show { with_path = false }]

type data = {
  uuid : unit uuid;
  mutable typ : typ;
}
[@@deriving show { with_path = false }]

let data' () = { uuid = uuid (); typ = TyUnknown }
let data_with_typ typ = { uuid = uuid (); typ }

type expr = Expr of (data[@opaque]) * tag * expr list
[@@deriving show { with_path = false }]

let get_data (Expr (dat, _, _)) = dat
let get_typ (Expr (dat, _, _)) = dat.typ
let get_tag (Expr (_, tag, _)) = tag
let get_children (Expr (_, _, children)) = children

type constructor = {
  name : name;
  index : int; (* index in the type (for tag) *)
}
[@@deriving show { with_path = false }]

type definition = {
  name : name;
  args : (name * typ) list;
  returns : typ;
  body : expr;
}
[@@deriving show { with_path = false }]

type program = {
  defs : definition list;
  externs : (string, typ) Hashtbl.t [@opaque];
  (* map constructor names to their tag numbers *)
  constructors : (name, int) Hashtbl.t [@opaque];
     (* given a list of types as arguments to some type,
     spit out all the inputs to the constructors
     this is used to determine the size needed to be
     allocated by a constructor upon its construction,
     as constructors like Nil need to allocate
     "more than obvious"

     TODO: this is totally a hack
     *)
  gen_type_sizes : name -> typ list -> typ list list;
}
[@@deriving show { with_path = false }]

let process_in_definitions func prog =
  let fold_def def = { def with body = func def.body } in
  { prog with defs = List.map fold_def prog.defs }

let process_in_definitions' func prog =
  let fold_def def = { def with body = func def def.body } in
  { prog with defs = List.map fold_def prog.defs }
