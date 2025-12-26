open Share.Uuid
open Share.Maybe

(* ideally these would be newtypes, but ocaml doesn't have those *)
type resolved = R of string [@@deriving show { with_path = false }]

let fresh_resolved =
  let i = ref (-10) in
  fun () ->
    decr i;
    R (string_of_int !i)

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
  | TyMeta of 'a meta ref
[@@deriving show { with_path = false }]

and 'a field = 'a * 'a typ [@@deriving show { with_path = false }]

let rec regeneralize (nw : unit -> 'a) (ty : 'a typ) =
  let f = regeneralize nw in
  match ty with
  | TyTuple t -> TyTuple (List.map f t)
  | TyArrow (a, b) -> TyArrow (f a, f b)
  | TyCustom (c, t) -> TyCustom (c, List.map f t)
  | TyRef r -> TyRef (f r)
  | TyMeta m ->
      TyMeta
        (ref
           begin
             match !m with
             | Resolved m -> Resolved m
             | Unresolved ->
                 m := Resolved (TyPoly (nw ()));
                 Resolved (TyMeta m)
           end)
  | _ -> ty

let rec force (t : 'a typ) : 'a typ =
  match (t : 'a typ) with
  | TyTuple t -> TyTuple (List.map force t)
  | TyArrow (a, b) -> TyArrow (force a, force b)
  | TyCustom (a, b) -> TyCustom (a, List.map force b)
  | TyRef a -> TyRef (force a)
  | TyMeta m -> begin
      match !m with Unresolved -> t | Resolved t -> force t
    end
  | _ -> t

let get_polys t =
  let rec g t =
    match force t with
    | TyTuple t -> List.map g t |> List.flatten
    | TyArrow (a, b) -> g a @ g b
    | TyCustom (_, t) -> List.map g t |> List.flatten
    | TyRef r -> g r
    | TyPoly a -> [ a ]
    | _ -> []
  in
  g t

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

let make_metas polys metas =
  List.map
    (fun x ->
      if not @@ List.mem x polys then
        [ (x, TyMeta (ref Unresolved)) ]
      else
        [])
    metas
  |> List.flatten

let to_metas polys t =
  let metas = get_polys t in
  let map = make_metas polys metas in
  instantiate map t

let to_metas' polys t =
  let metas = get_polys t in
  let map = make_metas polys metas in
  (instantiate map t, map)

let rec back_to_polys gen t =
  let f = back_to_polys gen in
  match force t with
  | TyTuple t -> TyTuple (List.map f t)
  | TyArrow (q, w) -> TyArrow (f q, f w)
  | TyCustom (x, t) -> TyCustom (x, List.map f t)
  | TyRef t -> TyRef (f t)
  | TyMeta m -> begin
      match !m with
      | Resolved _ -> failwith "impossible"
      | Unresolved ->
          m := Resolved (TyPoly (gen ()));
          t
    end
  | t -> t

let rec subst_polys (map : ('a * 'a typ) list) (x : 'a typ) : 'a typ =
  let f = subst_polys map in
  match force (x : 'a typ) with
  | TyTuple t -> TyTuple (List.map f t)
  | TyArrow (a, b) -> TyArrow (f a, f b)
  | TyPoly k -> begin
      match List.assoc_opt k map with Some t -> t | None -> x
    end
  | TyCustom (a, bnd) -> TyCustom (a, List.map f bnd)
  | TyRef r -> TyRef (f r)
  | TyMeta m -> x
  | _ -> x

let rec match_polys bef aft : 'a list =
  match (force bef, force aft) with
  | a, b when a = b -> []
  | TyTuple a, TyTuple b -> List.flatten (List.map2 match_polys a b)
  | TyArrow (a, b), TyArrow (q, w) ->
      match_polys a q @ match_polys b w
  | TyCustom (_, xs), TyCustom (_, ys) ->
      (* we assume it's all correct *)
      List.flatten (List.map2 match_polys xs ys)
  | TyRef a, TyRef b -> match_polys a b
  | TyPoly a, b | b, TyPoly a -> [ (a, b) ]
  | a, b ->
      print_endline (show_typ pp_resolved a);
      print_endline (show_typ pp_resolved b);
      failwith "match_polys bad"

type literal =
  | LBool of bool
  | LInt of string
[@@deriving show { with_path = false }]

type 'a case =
  | CaseWild
  | CaseVar of 'a
  | CaseTuple of 'a case list
  | CaseCtor of 'a * 'a case list
  | CaseLit of literal
[@@deriving show { with_path = false }]

let rec case_names (c : 'a case) : 'a list =
  match c with
  | CaseWild -> []
  | CaseVar c -> [ c ]
  | CaseTuple t -> List.flatten (List.map case_names t)
  | CaseCtor (c, t) -> List.flatten (List.map case_names t)
  | CaseLit _ -> []

type 'a data = {
  uuid : 'a uuid;
  mutable counter : int;
  (* file line col *)
  span : (string * int * int) option;
}
[@@deriving show { with_path = false }]

let data () = { uuid = Share.Uuid.uuid (); counter = 0; span = None }
let data_of uuid = { uuid; counter = 0; span = None }

let update_data_uuid data nw =
  { data with uuid = uuid_set_snd nw data.uuid }

type binop =
  | Add
  | Sub
  | Mul
  | Div
  | LAnd
  | LOr
  | Lt
  | Gt
  | LtEq
  | GtEq
  | Eq
[@@deriving show { with_path = false }]

type unaryop =
  | Negate
  | BNegate
  | Ref
  | IsConstr of string
  | GetRecField of string
  | Project of int
  | GetConstrField of int
[@@deriving show { with_path = false }]

type ('a, 'b) expr =
  | Var of 'b data * 'a
  (* For monomorphization
     Arguably this should warrant another AST but I don't think
     that's really needed
   *)
  | MGlobal of 'b data * 'a typ uuid * 'a
  | MLocal of 'b data * 'a
  | Int of 'b data * string
  | String of 'b data * string
  | Char of 'b data * string
  | Float of 'b data * string
  | Bool of 'b data * bool
  | LetIn of
      'b data
      * 'a case
      * 'a typ option
      * ('a, 'b) expr
      * ('a, 'b) expr
  | Seq of 'b data * ('a, 'b) expr * ('a, 'b) expr
  | Funccall of 'b data * ('a, 'b) expr * ('a, 'b) expr
  | Binop of 'b data * binop * ('a, 'b) expr * ('a, 'b) expr
  | UnaryOp of 'b data * unaryop * ('a, 'b) expr
  | Lambda of 'b data * 'a * 'a typ option * ('a, 'b) expr
  | Tuple of 'b data * ('a, 'b) expr list
  | Annot of 'b data * ('a, 'b) expr * 'a typ
  | Match of 'b data * ('a, 'b) expr * ('a case * ('a, 'b) expr) list
  | Modify of 'b data * 'a * ('a, 'b) expr
  | Record of 'b data * 'a * ('a * ('a, 'b) expr) list
[@@deriving show { with_path = false }]

let get_data (e : ('a, 'b) expr) : 'b data =
  match e with
  | MLocal (i, _)
  | MGlobal (i, _, _)
  | Var (i, _)
  | Int (i, _)
  | String (i, _)
  | Char (i, _)
  | Float (i, _)
  | Bool (i, _)
  | LetIn (i, _, _, _, _)
  | Seq (i, _, _)
  | Funccall (i, _, _)
  | Binop (i, _, _, _)
  | Lambda (i, _, _, _)
  | Tuple (i, _)
  | Annot (i, _, _)
  | Match (i, _, _)
  | UnaryOp (i, _, _)
  | Modify (i, _, _)
  | Record (i, _, _) ->
      i

let get_uuid x = (get_data x).uuid

let data_transform (type a b) (f : a data -> b data) expr =
  let rec go e =
    match e with
    | MLocal (d, s) -> MLocal (f d, s)
    | MGlobal (d, p, s) -> MGlobal (f d, p, s)
    | Var (i, s) -> Var (f i, s)
    | Int (i, s) -> Int (f i, s)
    | String (i, s) -> String (f i, s)
    | Char (i, s) -> Char (f i, s)
    | Float (i, s) -> Float (f i, s)
    | Bool (i, s) -> Bool (f i, s)
    | LetIn (i, c, ty, e1, e2) -> LetIn (f i, c, ty, go e1, go e2)
    | Seq (i, a, b) -> Seq (f i, go a, go b)
    | Funccall (i, a, b) -> Funccall (f i, go a, go b)
    | Binop (i, b, e1, e2) -> Binop (f i, b, go e1, go e2)
    | Lambda (i, nm, t, e) -> Lambda (f i, nm, t, go e)
    | Tuple (i, s) -> Tuple (f i, List.map go s)
    | Annot (i, e, t) -> Annot (f i, go e, t)
    | Match (i, e, cs) ->
        Match (f i, go e, List.map (fun (b, a) -> (b, go a)) cs)
    | UnaryOp (i, a, e) -> UnaryOp (f i, a, go e)
    | Modify (i, a, e) -> Modify (f i, a, go e)
    | Record (i, a, cs) ->
        Record (f i, a, List.map (fun (a, b) -> (a, go b)) cs)
  in
  go expr

let expr_uuid_set_snd v expr =
  let f d = { d with uuid = Share.Uuid.uuid_set_snd v d.uuid } in
  data_transform f expr

type 'a typdef_case =
  | Record of 'a field list
  | Sum of ('a * 'a typ list) list
[@@deriving show { with_path = false }]

let rec typ_list_to_typ (t : 'a typ list) : 'a typ =
  match t with
  | [] -> failwith "empty typ"
  | [ x ] -> x
  | x :: xs -> TyArrow (x, typ_list_to_typ xs)

let typ_to_args_ret (typ : 'a typ) : 'a typ list * 'a typ =
  let rec go ty =
    match ty with
    | TyArrow (a, TyArrow (b, c)) ->
        let rest, tl = go (TyArrow (b, c)) in
        (a :: rest, tl)
    | TyArrow (a, b) -> ([ a ], b)
    | other -> ([], other)
  in
  go typ

(* TODO: support GADTs*)
type 'a typdef = {
  data : unit data;
  name : 'a;
  args : 'a list;
  content : 'a typdef_case;
}
[@@deriving show { with_path = false }]

let typdef_and_ctor_to_typ (t : 'a typdef) (i : 'a) : 'a typ =
  match t.content with
  | Record _ -> failwith "shouldn't be record"
  | Sum s ->
      let ctor = List.assoc i s in
      let custom =
        TyCustom (t.name, List.map (fun x -> TyPoly x) t.args)
      in
      typ_list_to_typ (ctor @ [ custom ])

type ('a, 'b, 'p) definition = {
  data : 'b data;
  name : 'a;
  typeargs : 'a list;
  args : ('a * 'a typ) list;
  return : 'a typ;
  body : (('a, 'b) expr, 'p) maybe;
}
[@@deriving show { with_path = false }]

let forget_body : ('a, 'b, yes) definition -> ('a, 'b, no) definition
    =
 fun x -> { x with body = Nothing }

let conjur_body :
    ('a, 'b, 'd) definition ->
    ('a, 'b) expr ->
    ('a, 'b, yes) definition =
 fun x b -> { x with body = Just b }

let definition_type (type a) (d : ('a, 'b, a) definition) : 'a typ =
  List.fold_right
    (fun (_, ty) acc -> TyArrow (ty, acc))
    d.args d.return

type ('a, 'b) toplevel =
  | Typdef of 'a typdef
  | Definition of ('a, 'b, yes) definition
[@@deriving show { with_path = false }]
