open Share.Uuid
open Share.Maybe

(* ideally these would be newtypes, but ocaml doesn't have those *)
type resolved = R of string [@@deriving show { with_path = false }]

let fresh_resolved =
  let i = ref (-10) in
  fun () ->
    decr i;
    string_of_int !i

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
  | TyAssoc of 'a trait_bound * 'a (* <A as T a b, whatever>::B *)
  | TyRef of 'a typ (* mutability shock horror *)
  | TyMeta of 'a meta ref
[@@deriving show { with_path = false }]

and 'a field = 'a * 'a typ [@@deriving show { with_path = false }]

and 'a trait_bound =
  uuid
  * 'a
  (* unique id, trait name*)
  * ('a * 'a typ) list (* args *)
  * ('a * 'a typ) list (* assocs *)
[@@deriving show { with_path = false }]

let do_within_trait_bound (fn : 'a typ -> 'a typ) (t : 'a trait_bound)
    : 'a trait_bound =
  let uuid, name, args, assoc = t in
  let snd_f (a, b) = (a, fn b) in
  (uuid, name, List.map snd_f args, List.map snd_f assoc)

let do_within_trait_bound' (fn : 'a typ -> 'b) (t : 'a trait_bound) :
    'b list =
  let uuid, name, args, assoc = t in
  List.map (fun x -> fn (snd x)) args
  @ List.map (fun x -> fn (snd x)) assoc

let do_within_trait_bound'2 (fn : 'a typ -> 'a typ -> 'b) q w :
    'b list =
  let uuid, name, args1, assoc1 = q in
  let uuid, name, args2, assoc2 = w in
  List.map2 (fun x y -> fn (snd x) (snd y)) args1 args2
  @ List.map2 (fun x y -> fn (snd x) (snd y)) assoc1 assoc2

let rec regeneralize (nw : unit -> 'a) (ty : 'a typ) =
  let f = regeneralize nw in
  match ty with
  | TyTuple t -> TyTuple (List.map f t)
  | TyArrow (a, b) -> TyArrow (f a, f b)
  | TyCustom (c, t) -> TyCustom (c, List.map f t)
  | TyAssoc (trt, a) -> TyAssoc (do_within_trait_bound f trt, a)
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
  | TyAssoc (bd, a) -> TyAssoc (do_within_trait_bound force bd, a)
  | _ -> t

type ptr = int64

let ptr_of (x : 'a) : ptr =
  (* SEGFAULTS IF CALLED ON AN INT *)
  if Obj.is_int (Obj.repr x) then
    failwith "ptr_of integer"
  else
    Obj.magic x

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
  | TyAssoc (bnd, a) -> TyAssoc (do_within_trait_bound f bnd, a)
  | TyRef r -> TyRef (f r)
  | TyMeta m -> x
  | _ -> x

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

let data () = { uuid = Share.Uuid.uuid (); span = None }

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

(* TODO: support GADTs*)
type 'a typdef = {
  data : data;
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
  (* TODO: support stuff of the form impl<A> T<A>*)
  data : data;
  parent : 'a;
  args : ('a * 'a typ) list;
  assocs : ('a * 'a typ) list;
  (* we give each impl function a unique id alongside it's
     name predefined from the trait
  *)
  impls : (uuid * ('a, yes) definition) list;
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
