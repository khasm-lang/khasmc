open Exp

type khagmid = int
type unboxed = Int' of string | String' of string | Float' of string

type khagmexpr =
  | Val of khagmid
  | Unboxed of unboxed
  | Tuple of khagmexpr list
  | Call of khagmexpr * khagmexpr
  | Eval of khagmexpr

type khagmtop =
  | Let of khagmid * khagmid list * khagmexpr
  | Extern of khagmid * string
