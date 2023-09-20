open Exp

type id = int [@@deriving show { with_path = false }]

type value =
  | Val of id
  | Int of string
  | String of string
  | Float of string
  | Bool of string
  | Tuple of value list
[@@deriving show { with_path = false }]

type special = TupAcc of int [@@deriving show { with_path = false }]

type khagmexpr =
  | Fail of string
  | LetInVal of id * value
  | LetInCall of id * id * value list  (** ret, func, vals *)
  | IfElse of id * id * khagmexpr list * khagmexpr list  (** ret, cond, e1, e2*)
  | Special of id * value * special
  | SubExpr of id * khagmexpr list
  | CheckCtor of id * id * int
  | Return of id
  | Ref of id
  | Unref of id
[@@deriving show { with_path = false }]

type khagmtop =
  | Let of id * id list * khagmexpr list
  | Ctor of id * int  (** name * arity *)
  | Extern of id * int * string
  | Noop
[@@deriving show { with_path = false }]

type khagm = khagmtop list * Kir.kir_table
[@@deriving show { with_path = false }]
