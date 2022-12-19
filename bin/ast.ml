type typesig =
  | TSBase of string
  | TSApp of typesig * string
  | TSMap of typesig * typesig
  | TSForall of string * typesig
  | TSTuple of typesig list

[@@deriving show {with_path = false}, eq]



let brac x = "(" ^ x ^ ")"

let rec by_sep x y =
  match x with
  | [] -> ""
  | [x] -> x
  | x :: xs -> x ^ y ^ by_sep xs y 

let rec pshow_typesig ts =
  match ts with
  | TSBase(x) -> x
  | TSApp(x, y) -> brac (pshow_typesig x) ^ y
  | TSMap(x, y) ->
     pshow_typesig x ^ " -> " ^ brac (pshow_typesig y)
  | TSForall(x, y) -> "∀" ^ x ^ ", " ^ pshow_typesig y
  | TSTuple(x) -> brac (by_sep (List.map pshow_typesig x) ", ")

let str_of_typesig x = pshow_typesig x

type fident =
  | Bot of string
  | Mod of string * fident (* mod.x *)
  | Struc of string * fident (* struc:x *)
[@@deriving show {with_path = false}, eq]



let rec unqual x =
  match x with
  | Bot(y) -> y
  | Mod(_, y) -> unqual y
  | Struc(_, y) -> unqual y

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
  let reg = Str.regexp "([:] | [.])" in
  let whole = Str.full_split reg s in
  build_fullident whole

let rec mod_from_list l e =
  match l with
  | [x] -> Mod(x, Bot(e))
  | x :: xs -> Mod(x, mod_from_list xs e)
  | [] -> Bot(e)

let rec str_of_fident f =
  match f with
  | Bot(x) -> x
  | Mod(x, y) -> x ^ "." ^ str_of_fident y
  | Struc(x, y) -> x ^ ":" ^ str_of_fident y

type kident = string
[@@deriving show {with_path = false}]


type tdecl = kident * typesig
[@@deriving show {with_path = false}]


type kbase =
  | Ident of kident
  | Int of string
  | Float of string
  | Str of string
  | Tuple of kexpr list
  | True
  | False
[@@deriving show {with_path = false}]




and unop = string
[@@deriving show {with_path = false}]

and binop = string
[@@deriving show {with_path = false}]


and kexpr =
  | Base of kbase
  | Paren of kexpr
  | FCall of kexpr * kexpr
  | UnOp of unop * kexpr
  | BinOp of kexpr * binop * kexpr
  | LetIn of kident * kexpr * kexpr
  | IfElse of kexpr * kexpr * kexpr
  | Join of kexpr * kexpr (* expr1; expr2; expr3, rightassoc*)
  | Inst of kexpr * typesig
  | Lam of kident * kexpr
  | TypeLam of kident * kexpr
  | TupAccess of kexpr * int
  | AnnotLet of kident * typesig * kexpr * kexpr
  | AnnotLam of kident * typesig * kexpr


[@@deriving show {with_path = false}]


and kass = kident * kident list *  kexpr
[@@deriving show {with_path = false}]

and toplevel =
  | TopAssign of tdecl * kass
[@@deriving show {with_path = false}]

and program = | Program of toplevel list
[@@deriving show {with_path = false}]
