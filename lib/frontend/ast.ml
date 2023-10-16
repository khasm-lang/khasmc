let khasm_default_types = [ "int"; "float"; "string"; "bool" ]

type typeprim =
  | Basic of string
  | Bound of string
  | Param of int * string
[@@deriving show { with_path = false }]

type typesig =
  | TSBase of string
  | TSMeta of string
  | TSApp of typesig list * string
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
  | TSMeta x -> "$" ^ x
  | TSApp (x, y) ->
      brac (y ^ " " ^ String.concat " " @@ List.map pshow_typesig x)
  | TSMap (x, y) -> brac (pshow_typesig x) ^ " -> " ^ brac (pshow_typesig y)
  | TSForall (x, y) -> "forall " ^ x ^ ", " ^ pshow_typesig y
  | TSTuple x -> brac (by_sep (List.map pshow_typesig x) ", ")

let str_of_typesig x = pshow_typesig x

type info = {
  id : int;
  span : Errors.span; [@printer fun fmt span -> fprintf fmt "<span>"]
}
[@@deriving show { with_path = false }]

type info2 = info * info [@@deriving show { with_path = false }]
type info3 = info * info * info [@@deriving show { with_path = false }]
type info4 = info * info * info * info [@@deriving show { with_path = false }]

let dummyinfo = { id = 0; span = Errors.emptyspan }
let dummyinfo2 = (dummyinfo, dummyinfo)
let dummyinfo3 = (dummyinfo, dummyinfo, dummyinfo)
let dummyinfo4 = (dummyinfo, dummyinfo, dummyinfo, dummyinfo)
let idgen = ref 0

let getid () =
  let tmp = !idgen in
  idgen := tmp + 1;
  tmp

let mkinfo () = { id = getid (); span = Errors.emptyspan }
let info s = { id = getid (); span = s }
let info2 (a, b) = ({ id = getid (); span = a }, { id = getid (); span = b })

let info3 (a, b, c) =
  ( { id = getid (); span = a },
    { id = getid (); span = b },
    { id = getid (); span = c } )

let info4 (a, b, c, d) =
  ( { id = getid (); span = a },
    { id = getid (); span = b },
    { id = getid (); span = c },
    { id = getid (); span = d } )

type kident = string [@@deriving show { with_path = false }]

type kbase =
  | Ident of info * kident
  | Int of string
  | Float of string
  | Str of string
  | Tuple of kexpr list
  | True
  | False
[@@deriving show { with_path = false }]

and matchpat =
  | MPWild
  | MPInt of kident
  | MPId of kident
  | MPApp of kident * matchpat list
  | MPTup of matchpat list
[@@deriving show { with_path = false }]

and kexpr =
  | Base of info * kbase
  | FCall of info * kexpr * kexpr
  | LetIn of info * kident * kexpr * kexpr
  | LetRecIn of info * typesig * kident * kexpr * kexpr
  | IfElse of info * kexpr * kexpr * kexpr
  | Join of info * kexpr * kexpr (* expr1; expr2; expr3, rightassoc*)
  | Lam of info * kident * kexpr
  | TypeLam of info * kident * kexpr
  | TupAccess of info * kexpr * int
  | AnnotLet of info * kident * typesig * kexpr * kexpr
  | AnnotLam of info * kident * typesig * kexpr
  | ModAccess of info * kident list * kident
  | Match of info * kexpr * (matchpat * kexpr) list
[@@deriving show { with_path = false }]

and adt_pattern = {
  head : kident;  (** Constructor name *)
  args : typesig list;  (** list of types to be input to ctor *)
  typ : (typesig, unit) result;  (** Type ctor returns *)
}
[@@deriving show { with_path = false }]

and toplevel =
  | TopAssign of info4 * kident * typesig * kident list * kexpr
  | TopAssignRec of info4 * kident * typesig * kident list * kexpr
  | Extern of info3 * kident * int * typesig
  | IntExtern of info4 * kident * kident * int * typesig
  | SimplModule of info2 * kident * toplevel list
  | Bind of info3 * kident * kident list * kident
  | Open of info * kident
  | Typedecl of info3 * kident * kident list * adt_pattern list
  | Typealias of info3 * kident * kident list * typesig
[@@deriving show { with_path = false }]

and program = Program of toplevel list [@@deriving show { with_path = false }]

(*comes up surprisingly often*)
let rec get_pat_frees pat =
  match pat with
  | MPApp (_, t) | MPTup t -> List.concat_map get_pat_frees t
  | MPId t -> [ t ]
  | MPInt _ -> []
  | MPWild -> []

(* The following two are basically just α-renaming *)
let base_subs i b x y =
  match b with
  | Ident (i', b') ->
      if b' = x then
        Base (i, Ident (i', y))
      else
        Base (i, b)
  | _ -> Base (i, b)

let rec esubs expr x y =
  match expr with
  | Base (i, b) -> base_subs i b x y
  | FCall (i, f, x') -> FCall (i, esubs f x y, esubs x' x y)
  | LetIn (i, id, e1, e2) ->
      if id <> x then
        LetIn (i, id, esubs e1 x y, esubs e2 x y)
      else
        expr
  | AnnotLet (i, id, ts, e1, e2) ->
      if id <> x then
        AnnotLet (i, id, ts, esubs e1 x y, esubs e2 x y)
      else
        expr
  | LetRecIn (i, ts, id, e1, e2) ->
      if id <> x then
        LetRecIn (i, ts, id, esubs e1 x y, esubs e2 x y)
      else
        expr
  | IfElse (i, c, e1, e2) -> IfElse (i, esubs c x y, esubs e1 x y, esubs e2 x y)
  | Join (info, e1, e2) -> Join (info, esubs e1 x y, esubs e2 x y)
  | Lam (i, x', e) ->
      if x' <> x then
        Lam (i, x', esubs e x y)
      else
        expr
  | TypeLam (i, t, e) -> TypeLam (i, t, esubs e x y)
  | TupAccess (i, e, i') -> TupAccess (i, esubs e x y, i')
  | AnnotLam (i, x', ts, e) ->
      if x' <> x then
        AnnotLam (i, x', ts, esubs e x y)
      else
        expr
  | ModAccess _ -> expr
  | Match (i, p, ps) ->
      Match
        ( i,
          p,
          List.map
            (fun (p, e) ->
              if not (List.mem x (get_pat_frees p)) then
                (p, esubs e x y)
              else
                (p, e))
            ps )

let get_span (expr : kexpr) =
  match expr with
  | Base (t, _)
  | FCall (t, _, _)
  | LetIn (t, _, _, _)
  | LetRecIn (t, _, _, _, _)
  | IfElse (t, _, _, _)
  | Join (t, _, _)
  | Lam (t, _, _)
  | TypeLam (t, _, _)
  | TupAccess (t, _, _)
  | AnnotLet (t, _, _, _, _)
  | AnnotLam (t, _, _, _)
  | ModAccess (t, _, _)
  | Match (t, _, _) ->
      t.span
