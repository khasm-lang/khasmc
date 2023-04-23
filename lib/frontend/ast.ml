type typesig =
  | TSBase of string
  | TSMeta of string
  | TSApp of typesig * string
  | TSMap of typesig * typesig
  | TSForall of string * typesig
  | TSTuple of typesig list
[@@deriving show { with_path = false }, eq]

let tsBottom = TSTuple []
let brac x = "(" ^ x ^ ")"

let rec by_sep x y =
  match x with [] -> "" | [ x ] -> x | x :: xs -> x ^ y ^ by_sep xs y

let rec pshow_typesig ts =
  match ts with
  | TSBase x -> x
  | TSMeta x -> x
  | TSApp (x, y) -> brac (pshow_typesig x) ^ " " ^ y
  | TSMap (x, y) -> brac (pshow_typesig x) ^ " -> " ^ brac (pshow_typesig y)
  | TSForall (x, y) -> "∀" ^ x ^ ", " ^ pshow_typesig y
  | TSTuple x -> brac (by_sep (List.map pshow_typesig x) ", ")

let str_of_typesig x = pshow_typesig x

type info = { id : int; complex : int } [@@deriving show { with_path = false }]

let dummy_info () = { id = -1; complex = -2 }
let idgen = ref 0

let getid () =
  let tmp = !idgen in
  idgen := tmp + 1;
  tmp

let mkinfo () = { id = getid (); complex = -1 }

type kident = string [@@deriving show { with_path = false }]
type tdecl = kident * typesig [@@deriving show { with_path = false }]

type kbase =
  | Ident of info * kident
  | Int of string
  | Float of string
  | Str of string
  | Tuple of kexpr list
  | True
  | False
[@@deriving show { with_path = false }]

and kexpr =
  | Base of info * kbase
  | FCall of info * kexpr * kexpr
  | LetIn of info * kident * kexpr * kexpr
  | LetRecIn of info * typesig * kident * kexpr * kexpr
  | IfElse of info * kexpr * kexpr * kexpr
  | Join of info * kexpr * kexpr (* expr1; expr2; expr3, rightassoc*)
  | Inst of info * kexpr * typesig
  | Lam of info * kident * kexpr
  | TypeLam of info * kident * kexpr
  | TupAccess of info * kexpr * int
  | AnnotLet of info * kident * typesig * kexpr * kexpr
  | AnnotLam of info * kident * typesig * kexpr
  | ModAccess of info * kident list * kident
[@@deriving show { with_path = false }]

and kass = kident * kident list * kexpr [@@deriving show { with_path = false }]

and toplevel =
  | TopAssign of tdecl * kass
  | TopAssignRec of tdecl * kass
  | Extern of kident * int * typesig
  | IntExtern of kident * kident * int * typesig
  | SimplModule of kident * toplevel list
  | Bind of kident * kident list * kident
  | Open of kident
[@@deriving show { with_path = false }]

and program = Program of toplevel list [@@deriving show { with_path = false }]

let base_subs i b x y =
  match b with
  | Ident (i', b') -> if b' = x then Base (i, Ident (i', y)) else Base (i, b)
  | _ -> Base (i, b)

let rec esubs expr x y =
  match expr with
  | Base (i, b) -> base_subs i b x y
  | FCall (i, f, x') -> FCall (i, esubs f x y, esubs x' x y)
  | LetIn (i, id, e1, e2) ->
      if id <> x then LetIn (i, id, esubs e1 x y, esubs e2 x y) else expr
  | AnnotLet (i, id, ts, e1, e2) ->
      if id <> x then AnnotLet (i, id, ts, esubs e1 x y, esubs e2 x y) else expr
  | LetRecIn (i, ts, id, e1, e2) ->
      if id <> x then LetRecIn (i, ts, id, esubs e1 x y, esubs e2 x y) else expr
  | IfElse (i, c, e1, e2) -> IfElse (i, esubs c x y, esubs e1 x y, esubs e2 x y)
  | Join (info, e1, e2) -> Join (info, esubs e1 x y, esubs e2 x y)
  | Lam (i, x', e) -> if x' <> x then Lam (i, x', esubs e x y) else expr
  | TypeLam (i, t, e) -> TypeLam (i, t, esubs e x y)
  | TupAccess (i, e, i') -> TupAccess (i, esubs e x y, i')
  | AnnotLam (i, x', ts, e) ->
      if x' <> x then AnnotLam (i, x', ts, esubs e x y) else expr
  | ModAccess _ -> expr
  | Inst _ -> expr

let getinfo expr =
  match expr with
  | Base (inf, _)
  | FCall (inf, _, _)
  | LetIn (inf, _, _, _)
  | IfElse (inf, _, _, _)
  | Join (inf, _, _)
  | Inst (inf, _, _)
  | Lam (inf, _, _)
  | TypeLam (inf, _, _)
  | TupAccess (inf, _, _)
  | AnnotLet (inf, _, _, _, _)
  | AnnotLam (inf, _, _, _)
  | ModAccess (inf, _, _)
  | LetRecIn (inf, _, _, _, _) ->
      inf
