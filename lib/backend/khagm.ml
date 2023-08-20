open Exp

type khagmid = int [@@deriving show { with_path = false }]

type unboxed =
  | Int' of string
  | String' of string
  | Float' of string
  | Bool' of bool
[@@deriving show { with_path = false }]

type khagmexpr =
  | Val of khagmid
  | Unboxed of unboxed
  | Tuple of khagmexpr list
  | Call of khagmexpr * khagmexpr
  | Seq of khagmexpr * khagmexpr
  | Let of khagmid * khagmexpr * khagmexpr
  | IfElse of khagmexpr * khagmexpr * khagmexpr
[@@deriving show { with_path = false }]

type khagmtop =
  | Let of khagmid * khagmid list * khagmexpr
  | Extern of khagmid * int * string
  | Noop
[@@deriving show { with_path = false }]

type khagm = khagmtop list * Kir.transtable
[@@deriving show { with_path = false }]
