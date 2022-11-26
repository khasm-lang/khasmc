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

type fident =
  | Bot of string
  | Mod of string * fident (* mod::x *)
  | Struc of string * fident (* struc.x *)
[@@deriving show {with_path = false}, eq]

exception Impossible of string 

let fullident_ensure_str x =
  match x with
  | Str.Text(y) -> y
  | Str.Delim(_) -> raise (Impossible("fullident_ensure_str"))

let fullident_ensure_delim x =
  match x with
  | Str.Text(_) -> raise (Impossible("fullident_ensure_delim"))
  | Str.Delim(y) -> y

let rec build_fullident x =
  match x with
  | [] -> raise (Impossible("build_fullident 1"))
  | [y] -> Bot(fullident_ensure_str y)
  | y1 :: y2 :: ys ->
     match fullident_ensure_delim y2 with
     | ":" -> Struc(fullident_ensure_str y1, build_fullident ys)
     | "." -> Mod(fullident_ensure_str y1, build_fullident ys)
     | _ -> raise (Impossible("build_fullident 2"))
       
let process_fullident s =
  let reg = Str.regexp "([:][:] | [.])" in
  let whole = Str.full_split reg s in
  build_fullident whole

let rec mod_from_list l e =
  match l with
  | [x] -> Mod(x, Bot(e))
  | x :: xs -> Mod(x, mod_from_list xs e)
  | [] -> Bot(e)

type kident = string
[@@deriving show {with_path = false}]


type tdecl = kident * typesig
[@@deriving show {with_path = false}]


type kbase =
  | Ident of fident
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
