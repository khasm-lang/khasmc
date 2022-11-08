

type ktype =
  | KTypeBasic of string
[@@deriving show {with_path = false}]

type typesig =
  | TSBase of ktype
  | TSMap of typesig * typesig
  | TSForall of string list * typesig
[@@deriving show {with_path = false}]

type kident = string
[@@deriving show {with_path = false}]


type tdecl =
  | TDecl of kident * typesig
[@@deriving show {with_path = false}]


type kbase =
  | Ident of kident
  | Int of string
  | Float of string
  | Str of string
[@@deriving show {with_path = false}]


type binop = | ADD
[@@deriving show {with_path = false}]


type kexpr =
  | Base of kbase
  | Paren of kexpr
  | FCall of kexpr * kexpr list
  | BinOp of kexpr * binop * kexpr
  | LetIn of kident * kexpr * kexpr
  | IfElse of kexpr * kexpr * kexpr
[@@deriving show {with_path = false}]

type kass =
  | KAss of kident * kident list *  kexpr
[@@deriving show {with_path = false}]

type toplevel =
  | TopAssign of kass
  | TopTDecl of tdecl
[@@deriving show {with_path = false}]

type program = | Program of toplevel list
[@@deriving show {with_path = false}]
