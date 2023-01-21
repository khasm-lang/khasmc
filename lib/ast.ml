type typesig =
  | TSBottom
  | TSBase of string
  | TSMeta of string
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
  | TSBottom -> "()"
  | TSBase(x) -> x
  | TSMeta(x) -> x
  | TSApp(x, y) -> brac (pshow_typesig x) ^ y
  | TSMap(x, y) ->
     brac (pshow_typesig x) ^ " -> " ^ brac (pshow_typesig y)
  | TSForall(x, y) -> "∀" ^ x ^ ", " ^ pshow_typesig y
  | TSTuple(x) -> brac (by_sep (List.map pshow_typesig x) ", ")

let str_of_typesig x = pshow_typesig x

type info = {
    id: int;
  }
[@@deriving show {with_path=false}]

let dummy_info () = {id = -1}

let idgen = ref 0
let getid () =
  let tmp = !idgen in 
	    idgen := tmp + 1;
	    tmp
let mkinfo () = {id = getid()}

open Fident


type kident = string
[@@deriving show {with_path = false}]


type tdecl = kident * typesig
[@@deriving show {with_path = false}]


type kbase =
  | Ident of info * kident
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
  | Base of info * kbase
  | FCall of info * kexpr * kexpr
  | LetIn of info *  kident * kexpr * kexpr
  | IfElse of info *  kexpr * kexpr * kexpr
  | Join of info *  kexpr * kexpr (* expr1; expr2; expr3, rightassoc*)
  | Inst of info *  kexpr * typesig
  | Lam of  info * kident * kexpr
  | TypeLam of  info * kident * kexpr
  | TupAccess of  info * kexpr * int
  | AnnotLet of  info * kident * typesig * kexpr * kexpr
  | AnnotLam of  info * kident * typesig * kexpr


[@@deriving show {with_path = false}]


and kass = kident * kident list *  kexpr
[@@deriving show {with_path = false}]

and toplevel =
  | TopAssign of tdecl * kass
  | Extern of kident * typesig
[@@deriving show {with_path = false}]

and program = | Program of toplevel list
[@@deriving show {with_path = false}]


