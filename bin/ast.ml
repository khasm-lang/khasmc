type ktype =
  | KTypeBasic of string
  | KTypeApp of typesig * string
[@@deriving show {with_path = false}, eq]

and typesig =
  | TSBase of ktype
  | TSMap of typesig * typesig
  | TSForall of string list * typesig
  | TSTuple of typesig list
[@@deriving show {with_path = false}, eq]

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
  | Tuple of kexpr list
[@@deriving show {with_path = false}]


and unop = string
[@@deriving show {with_path = false}]

and binop = string
[@@deriving show {with_path = false}]


and kexpr =
  | Base of kbase
  | Paren of kexpr
  | FCall of kexpr * kexpr list
  | UnOp of unop * kexpr
  | BinOp of kexpr * binop * kexpr
  | LetIn of kident * kident list * kexpr * kexpr
  | IfElse of kexpr * kexpr * kexpr
  | Join of kexpr * kexpr (* expr1; expr2; expr3*)
[@@deriving show {with_path = false}]

and kass =
  | KAss of kident * kident list *  kexpr
[@@deriving show {with_path = false}]

and toplevel =
  | TopAssign of kass
  | TopTDecl of tdecl
[@@deriving show {with_path = false}]

and program = | Program of toplevel list
[@@deriving show {with_path = false}]
