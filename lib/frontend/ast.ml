open Share.Uuid
open Share.Maybe

(* ideally these would be newtypes, but ocaml doesn't have those *)
type resolved = R of int [@@deriving show { with_path = false }]

type unresolved = U of string
[@@deriving show { with_path = false }]

(* the long and short here is that we index
   stuff by identifier type, for ease of figuruing out
   whether something is resolved, unresolved, whatever
*)

type 'a meta =
  | Unresolved
  | Resolved of 'a typ
[@@deriving show { with_path = false }]

and 'a typ =
  | TyInt
  | TyString
  | TyChar
  | TyFloat
  | TyBool
  | TyTuple of 'a typ list
  | TyArrow of 'a typ * 'a typ
  | TyPoly of 'a
  | TyCustom of 'a * 'a typ list
  | TyRef of 'a typ (* mutability shock horror *)
  | TyMeta of 'a meta
[@@deriving show { with_path = false }]

and 'a field = Field of 'a * 'a typ
[@@deriving show { with_path = false }]

let rec force (t : 'a typ) : 'a typ =
  match (t : 'a typ) with
  | TyTuple t -> TyTuple (List.map force t)
  | TyArrow (a, b) -> TyArrow (force a, force b)
  | TyCustom (a, b) -> TyCustom (a, List.map force b)
  | TyRef a -> TyRef (force a)
  | TyMeta m -> begin
      match m with Unresolved -> t | Resolved t -> force t
    end
  | _ -> t

let rec instantiate (map : ('a * 'a typ) list) (t : 'a typ) : 'a typ =
  let f = instantiate map in
  match force (t : 'a typ) with
  | TyTuple t -> TyTuple (List.map f t)
  | TyArrow (a, b) -> TyArrow (f a, f b)
  | TyPoly x -> begin
      match List.assoc_opt x map with Some n -> n | None -> TyPoly x
    end
  | TyCustom (x, t) -> TyCustom (x, List.map f t)
  | TyRef t -> TyRef (f t)
  | _ -> t

type binop = Binop of string [@@deriving show { with_path = false }]

type 'a case =
  | CaseVar of 'a
  | CaseTuple of 'a case list
  | CaseCtor of 'a * 'a case list
[@@deriving show { with_path = false }]

type data = {
  uuid : uuid;
  (* file line col *)
  span : (string * int * int) option;
}
[@@deriving show { with_path = false }]

type 'a expr =
  | Var of data * 'a
  | Int of data * string
  | String of data * string
  | Char of data * string
  | Float of data * string
  | Bool of data * bool
  | LetIn of data * 'a case * 'a typ option * 'a expr * 'a expr
  | Seq of data * 'a expr * 'a expr
  | Funccall of data * 'a expr * 'a expr
  | Binop of data * binop
  | Lambda of data * 'a * 'a typ option * 'a expr
  | Tuple of data * 'a expr list
  | Annot of data * 'a expr * 'a typ
  | Match of data * 'a expr * ('a case * 'a expr) list
  | Project of data * 'a expr * int
  | Ref of data * 'a expr
  | Modify of data * 'a * 'a expr
  | Record of data * 'a * ('a * 'a expr) list
[@@deriving show { with_path = false }]

let get_uuid (e : 'a expr) : uuid =
  match e with
  | Var (i, _) -> i.uuid
  | Int (i, _) -> i.uuid
  | String (i, _) -> i.uuid
  | Char (i, _) -> i.uuid
  | Float (i, _) -> i.uuid
  | Bool (i, _) -> i.uuid
  | LetIn (i, _, _, _, _) -> i.uuid
  | Seq (i, _, _) -> i.uuid
  | Funccall (i, _, _) -> i.uuid
  | Binop (i, _) -> i.uuid
  | Lambda (i, _, _, _) -> i.uuid
  | Tuple (i, _) -> i.uuid
  | Annot (i, _, _) -> i.uuid
  | Match (i, _, _) -> i.uuid
  | Project (i, _, _) -> i.uuid
  | Ref (i, _) -> i.uuid
  | Modify (i, _, _) -> i.uuid
  | Record (i, _, _) -> i.uuid

type 'a typdef_case =
  | Record of 'a field list
  | Sum of ('a * 'a typ list) list
[@@deriving show { with_path = false }]

let rec typ_list_to_typ (t : 'a typ list) : 'a typ =
  match t with
  | [] -> failwith "empty typ"
  | [ x ] -> x
  | x :: xs -> TyArrow (x, typ_list_to_typ xs)

type 'a typdef = {
  data : data;
  name : 'a;
  args : 'a list;
  content : 'a typdef_case;
}
[@@deriving show { with_path = false }]

type 'a trait_bound =
  | Bound of 'a * 'a typ list (* trait name, "args" *)
[@@deriving show { with_path = false }]

type ('a, 'p) definition = {
  data : data;
  name : 'a;
  typeargs : 'a list;
  args : ('a * 'a typ) list;
  bounds : 'a trait_bound list;
  return : 'a typ;
  (* essentially for cross-compatibility with other structures *)
  body : ('a expr, 'p) maybe;
}
[@@deriving show { with_path = false }]

let forget_body : ('a, yes) definition -> ('a, no) definition =
 fun x -> { x with body = Nothing }

type 'a trait = {
  data : data;
  name : 'a;
  args : 'a list;
  assoc : 'a list;
  requirements : 'a trait_bound list;
  functions : ('a, no) definition list;
}
[@@deriving show { with_path = false }]

type 'a impl = {
  data : data;
  parent : uuid option;
  args : ('a * 'a typ) list;
  assocs : ('a * 'a typ) list;
  impls : ('a, yes) definition list;
}
[@@deriving show { with_path = false }]

let definition_type (type a) (d : ('a, a) definition) : 'a typ =
  List.fold_right
    (fun (_, ty) acc -> TyArrow (ty, acc))
    d.args d.return

type 'a toplevel =
  | Typdef of 'a typdef
  | Trait of 'a trait
  | Impl of 'a impl
  | Definition of ('a, yes) definition
[@@deriving show { with_path = false }]
