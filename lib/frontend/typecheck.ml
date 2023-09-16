open Exp
open Ast
open Uniq_typevars
open Hash
open Debug

(**
   This file serves as the typechecker for khasm programs.
   It handles everything related to ensuring a program is well typed.
   TODO: Split this into multiple files.

   In a long story short, this is a:
   - Bidirectional typechecking algorithm, based around:
   - Unification, using subs contexts instead of refs (TODO)
   - There's some special care needed to properly handle typechecking GADTs,
     but it's mostly self-contained - ie, the GADT code doesn't
     make anything else more complicated for any reason.

   At some point we need to think about how this can be properly split
   into multiple files without encountering ocaml's lack of multi-file
   mutual recursion as a major issue.

   The main problem I see is the plans in the future for linear types / a borrow checker,
   as that'll majorly complicate the typechecker even more then it already is,
   which is less then ideal seeing as it's almost 1,000 lines already :P
   
   Notes about this code:
   - Foralls are order dependent due to typelam elaboration
   - GADT typechecking is like, 70% confidence, not right 
*)


(** The global context for typechecking *)
type ctx = {
  (** type aliases that can be simplified *)
  aliases : (kident * kident list * typesig) list;
  (** bound functions / variables *)
  binds : (kident * typesig) list;
  (** types that can't be simplified *)
  types : typeprim list;
  (** constructors for types, used for match typechecking *)
  constrs : adt_pattern list;
  (** list of bound forall vars, uses for match typechecking (GADTs) *)
  bound_foralls : (kident * typesig) list;
}
[@@deriving show { with_path = false }]

let empty_typ_ctx () =
  {
    aliases = [];
    binds = [];
    types = List.map (fun x -> Basic x) khasm_default_types;
    constrs = [];
    bound_foralls = [];
  }

let add_alias ctx id args ts =
  { ctx with aliases = (id, args, ts) :: ctx.aliases }

let assume_typ ctx id ts =
  { ctx with binds = (id, make_uniq_ts ts None) :: ctx.binds }

let add_bound_typ ctx ts = { ctx with types = Bound ts :: ctx.types }
let add_param_typ ctx i ts = { ctx with types = Param (i, ts) :: ctx.types }
let add_constrs ctx pats = { ctx with constrs = pats @ ctx.constrs }

let add_bound_forall ctx f ty =
  { ctx with bound_foralls = (f, ty) :: ctx.bound_foralls }

(** Looks up a value in a context *)
let lookup ctx x =
  match List.find_opt (fun y -> fst y = x) ctx.binds with
  | None -> raise (NotFound (x ^ " not found in ctx"))
  | Some x -> snd x

let lookup_constr ctx x =
  match List.find_opt (fun y -> y.head = x) ctx.constrs with
  | None -> raise (NotFound ("Constructor " ^ x ^ " not found in ctx"))
  | Some x -> x

(** Checks whether a variable is free in a context *)
let rec occurs_ts s ts =
  (* checks whether a variable s is free in ts *)
  let res =
    match ts with
    | TSBase x -> x = s
    | TSMeta _ -> false
    | TSApp (x, _) -> List.mem true (List.map (occurs_ts s) x)
    | TSMap (x, y) -> occurs_ts s x || occurs_ts s y
    | TSForall (x, y) ->
        if x = s then
          false
        else
          occurs_ts s y
    | TSTuple x -> List.map (fun x -> occurs_ts s x) x |> List.mem true
  in
  res

(* some helpers around typeprims *)
let rec typeprim_is_basic tys x =
  match tys with
  | [] -> false
  | z :: zs -> (
      match z with
      | Basic y | Bound y ->
          if x = y then
            true
          else
            typeprim_is_basic zs x
      | Param (_, _) -> typeprim_is_basic zs x)

let rec typeprim_len tys x =
  match tys with
  | [] -> raise @@ TypeErr ("Type not found: " ^ x)
  | z :: zs -> (
      match z with
      | Bound _ | Basic _ -> typeprim_len zs x
      | Param (l, y) ->
          if y = x then
            l
          else
            typeprim_len zs x)
(** Ensures all sub-elements of a type are valid, eg. only using bound variables and typefuns*)
let rec validate_typ types ty =
  match ty with
  | TSBase x -> (
      match typeprim_is_basic types x with
      | true -> ty
      | false -> raise @@ TypeErr ("cannot validate: " ^ x))
  | TSMeta _ -> ty
  | TSApp (x, y) ->
      let n = typeprim_len types y in
      if n <> List.length x then
        raise @@ TypeErr ("Wrong number of args to type " ^ y)
      else
        TSApp (List.map (validate_typ types) x, y)
  | TSMap (x, y) -> TSMap (validate_typ types x, validate_typ types y)
  | TSForall (x, y) -> TSForall (x, validate_typ (Bound x :: types) y)
  | TSTuple l -> TSTuple (List.map (validate_typ types) l)

(* see below *)
let rec lift_ts_h t =
  match t with
  | TSBase _ -> (t, false)
  | TSMeta _ -> (t, false)
  | TSApp (x, y) ->
      let did = List.map lift_ts_h x in
      (TSApp (List.map fst did, y), List.mem true (List.map snd did))
  | TSMap (x, TSForall (f, y)) ->
      if occurs_ts f x then
        let left = lift_ts_h x in
        let right = lift_ts_h (TSForall (f, y)) in
        (TSMap (fst left, fst right), snd left || snd right)
      else
        (TSForall (f, TSMap (x, y)), true)
  | TSMap (x, y) ->
      let left = lift_ts_h x in
      let right = lift_ts_h y in
      (TSMap (fst left, fst right), snd left || snd right)
  | TSForall (x, TSMap (l, TSForall (y, r))) ->
      let l' = lift_ts_h l in
      let r' = lift_ts_h r in
      if occurs_ts y l then
        (TSForall (x, TSMap (fst l', TSForall (y, fst r'))), snd l' || snd r')
      else
        (TSForall (x, TSForall (y, TSMap (fst l', fst r'))), true)
  | TSForall (x, y) ->
      let r' = lift_ts_h y in
      (TSForall (x, fst r'), snd r')
  | TSTuple t ->
      let hm = List.map lift_ts_h t in
      (TSTuple (List.map fst hm), List.mem true (List.map snd hm))

(** lifts types like
     ∀ a, a -> (∀b, a -> b)
     to
     ∀a b, a -> a -> b
  *)
let rec lift_ts ts =
  match lift_ts_h ts with t, false -> t | t, true -> lift_ts t

(** Substitutes vars within a type *)
let rec subs typ nm newt =
  let ret =
    match typ with
    | TSBase x ->
        if x = nm then
          newt
        else
          typ
    | TSMeta _ -> typ
    | TSApp (x, y) -> TSApp (List.map (fun y -> subs y nm newt) x, y)
    | TSMap (l, r) -> TSMap (subs l nm newt, subs r nm newt)
    | TSForall (nm', ts) ->
        if nm' = nm then
          typ
        else
          TSForall (nm', subs ts nm newt)
    | TSTuple l -> TSTuple (List.map (fun x -> subs x nm newt) l)
  in
  ret

(** Substitues types within a type - not efficient *)
let rec subs_bad typ old new' =
  if typ = old then
    new'
  else
    match typ with
    | TSBase _ -> typ
    | TSMeta _ -> typ
    | TSApp (x, y) -> TSApp (List.map (fun y -> subs_bad y old new') x, y)
    | TSMap (l, r) -> TSMap (subs_bad l old new', subs_bad r old new')
    | TSForall (nm', ts) -> TSForall (nm', subs_bad ts old new')
    | TSTuple l -> TSTuple (List.map (fun x -> subs_bad x old new') l)

(** Substitutes multiple variables *)
let rec multisubs typ nms newts =
  match (nms, newts) with
  | [], [] -> typ
  | x :: xs, y :: ys -> multisubs (subs typ x y) xs ys
  | _, _ -> raise @@ Impossible "Unbalanced multisubs"

(** Takes an alias and a type, removing the alias from the type *)
let rec remove_alias (id, args, ts) typ =
  let ret =
    match typ with
    | TSBase a -> TSBase a
    | TSMeta a -> TSMeta a
    | TSApp (apps, fn) ->
        if fn = id then
          multisubs ts args apps
        else
          TSApp (List.map (remove_alias (id, args, ts)) apps, fn)
    | TSMap (f, x) ->
        TSMap (remove_alias (id, args, ts) f, remove_alias (id, args, ts) x)
    | TSForall (a, e) ->
        if a = id then
          TSForall (a, e)
        else
          TSForall (a, remove_alias (id, args, ts) e)
    | TSTuple l -> TSTuple (List.map (remove_alias (id, args, ts)) l)
  in
  ret

(** Wrapper to remove multiple aliases *)
let rec remove_aliases aliases ts =
  match aliases with
  | [] -> ts
  | x :: xs -> remove_aliases xs @@ remove_alias x ts

(* The unification section


*)

(** Contains a list of things that have been unified *)
type unify_ctx = { metas : (string * typesig) list }
[@@deriving show { with_path = false }]

let empty_unify_ctx () = { metas = [] }
let get_meta_opt ctx m = List.find_opt (fun x -> fst x = m) ctx.metas

(** Looks up meta variables in a context *)
let lookup_meta ctx m =
  match List.find_opt (fun x -> fst x = m) ctx.metas with
  | Some x -> Some (snd x)
  | None -> None

(** Instantiates foralls with metavars *)
let rec inst_meta tp orig meta =
  match tp with
  | TSBase x ->
      if x = orig then
        meta
      else
        tp
  | TSMeta _ -> tp
  | TSApp (ts, p) -> TSApp (List.map (fun y -> inst_meta y orig meta) ts, p)
  | TSMap (a, b) -> TSMap (inst_meta a orig meta, inst_meta b orig meta)
  | TSForall (f, x) ->
      if f = orig then
        tp
      else
        TSForall (f, inst_meta x orig meta)
  | TSTuple t -> TSTuple (List.map (fun x -> inst_meta x orig meta) t)

(** Instantiate all toplevel foralls *)
let rec inst_all ts =
  match ts with
  | TSForall (fv, bd) ->
      let meta = TSMeta (get_meta ()) in
      let new' = inst_meta bd fv meta in
      inst_all new'
  | TSApp (x, y) -> TSApp (x, y)
  | TSMap (x, y) -> TSMap (x, y)
  | TSBase x -> TSBase x
  | TSMeta x -> TSMeta x
  | TSTuple t -> TSTuple (List.map inst_all t)

(** eliminates unused forall variables *)
let rec elim_unused ts =
  let af =
    match ts with
    | TSBase _ -> ts
    | TSMeta _ -> ts
    | TSApp (a, b) -> TSApp (List.map elim_unused a, b)
    | TSMap (a, b) -> TSMap (elim_unused a, elim_unused b)
    | TSForall (a, b) ->
        if occurs_ts a b then
          TSForall (a, elim_unused b)
        else
          elim_unused b
    | TSTuple t -> TSTuple (List.map elim_unused t)
  in
  af

(** Unique list tools *)
let uniq_cons x xs =
  if List.mem x xs then
    xs
  else
    x :: xs

let mk_uniq_list xs = List.fold_right uniq_cons xs []
let combine_uniq x y = mk_uniq_list (x @ y)

(** Combines two context, ensuring no conflicts *)
let combine ctx1 ctx2 =
  let rec helper a b =
    match a with
    | [] -> b
    | x :: xs -> (
        match List.find_opt (fun y -> fst y = fst x) b with
        | None -> x :: helper xs b
        | Some k ->
            if snd k = snd x then
              helper xs b
            else
              raise
                (UnifyErr
                   ("metavariable " ^ fst x ^ " has both" ^ " type "
                   ^ pshow_typesig (snd k)
                   ^ " and "
                   ^ pshow_typesig (snd x)
                   ^ " within unification")))
  in
  { metas = helper ctx1.metas ctx2.metas }

(** Unifies a list *)
let rec unify_list ctx l1 l2 =
  match (l1, l2) with
  | [], [] -> (ctx, [])
  | x :: xs, y :: ys ->
      let tmp = unify ctx x y in
      let rest = unify_list (fst tmp) xs ys in
      (fst rest, snd tmp :: snd rest)
  | _, _ -> raise (UnifyErr "unequal tuple len")

(** TODO
   Credit to AradArbel10 on rpl

   if you're doing this in ocaml (or any language with ref cells) there's
   actually a really neat trick to optimize (and simplify 😄) metavars.
   {[
   type meta_state =
   | Solved of typ
   | Unsolved of name (* used for pretty printing *)
   and typ =
   | Arrow of typ * typ
   | ...
   | Meta of meta_state ref
    ]}
   so now the metavar is mutable, and when you solve it it gets
   "propagated up" automatically. actually implementing it introduces a
   couple of other subtleties but I think it's worth it.

{[
given f x
instantiate f with metavariables until reaching a non-forall type
check that f's type matches 'a -> 'b
unify x's type and 'a
apply that unification to 'b
return that type

    so, for example:

f : ∀'a, 'a -> 'a
x : float

f x
→ 
META_INST(f) : $m -> $m
→
unify LHS(f) and x, ∴ $m = float
→
apply to RHS(f)
∴ 
f : float -> float 
(in this application)
→
return RHS(f) : float
    ]}

    returns a tuple of env, typ

  Unifies two types, solving all needed metavars. *)
and unify ?loop ctx l r =
  let l = elim_unused @@ lift_ts l in
  let r = elim_unused @@ lift_ts r in
  let res =
    match (l, r) with
    | TSBase x, TSBase y ->
        if x = y then
          (ctx, TSBase x)
        else
          raise
            (UnifyErr
               ("can't unify " ^ x ^ " and " ^ y ^ " due to base mismatch"))
    | TSMap (a, b), TSMap (x, y) ->
        let lt = unify ctx a x in
        let rt = unify ctx b y in
        (combine (fst lt) (fst rt), TSMap (snd lt, snd rt))
    | TSApp (a, b), TSApp (x, y) ->
        if b <> y then
          raise
            (UnifyErr
               ("can't unify" ^ b ^ " and " ^ y
              ^ " due to type-level function mismatch"))
        else
          let t = List.map2 (unify ctx) a x in
          let rec get_ctx list =
            match list with
            | [] -> empty_unify_ctx ()
            | [ (c, _) ] -> c
            | (c1, _) :: xs -> combine c1 (get_ctx xs)
          in
          (get_ctx t, TSApp (List.map snd t, b))
    | TSForall (a, b), TSForall (x, y) ->
        (**
         TODO: show how this works
           better, at least
        *)
        let sub = subs y x (TSBase a) in
        let met = subs b a (TSMeta (get_meta ())) in
        let unf = unify ctx met sub in
        (fst unf, TSForall (a, snd unf))
    | TSTuple a, TSTuple x ->
        let tmp = unify_list ctx a x in
        (fst tmp, TSTuple (snd tmp))
    | TSMeta m, _ -> (
        match get_meta_opt ctx m with
        | Some x ->
            let typ = snd x in
            unify ctx typ r
        | None -> (combine { metas = [ (m, r) ] } ctx, r))
    | _, TSMeta m -> (
        match get_meta_opt ctx m with
        | Some x ->
            let typ = snd x in
            unify ctx typ l
        | None -> (combine { metas = [ (m, l) ] } ctx, l))
    | TSForall (id, ts), _ ->
        if not (occurs_ts id ts) then
          unify ctx (inst_all (TSForall (id, ts))) r
        else
          raise
            (UnifyErr
               ("Can't unify " ^ pshow_typesig l ^ " and " ^ pshow_typesig r
              ^ " due to forall mismatch"))
    | _, _ -> (
        match loop with
        | None -> unify ~loop:true ctx r l
        | Some _ ->
            raise
              (UnifyErr
                 ("Can't unify " ^ pshow_typesig l ^ " and " ^ pshow_typesig r))
        )
  in
  let res = (fst res, lift_ts (snd res)) in
  res

(** Applies a unify_ctx to a type *)
let rec apply_unify ctx tp =
  match tp with
  | TSBase _ -> tp
  | TSMeta t -> ( match lookup_meta ctx t with Some x -> x | None -> tp)
  | TSApp (f, x) -> TSApp (List.map (apply_unify ctx) f, x)
  | TSMap (a, b) -> TSMap (apply_unify ctx a, apply_unify ctx b)
  | TSForall (f, x) -> TSForall (f, apply_unify ctx x)
  | TSTuple t -> TSTuple (List.map (apply_unify ctx) t)

(** Converts all base types in something to metavariables - only occationaly useful*)
let rec all_base_to_meta tp =
  match tp with
  | TSBase _ -> TSMeta (get_meta ())
  | TSMeta _ -> tp
  | TSApp (f, x) -> TSApp (List.map all_base_to_meta f, x)
  | TSMap (a, b) -> TSMap (all_base_to_meta a, all_base_to_meta b)
  | TSForall (f, x) -> TSForall (f, all_base_to_meta x)
  | TSTuple t -> TSTuple (List.map all_base_to_meta t)

(* The check-infer section


*)

(** Infers the type of a base *)
let rec infer_base ctx tm =
  let typ =
    match tm with
    | Ident (inf, i) -> (
        try
          let ty = lookup_constr ctx i in
          let head =
            match ty.typ with
            | Error () -> raise @@ Impossible "infer_base"
            | Ok t -> t
          in
          match ty.args with
          | [] -> all_base_to_meta head
          | _ -> raise @@ Impossible "not seen"
        with _ ->
          let typ = lookup ctx i in
          Hash.add_typ inf.id typ;
          typ)
    | Int _ -> TSBase "int"
    | Float _ -> TSBase "float"
    | Str _ -> TSBase "string"
    | Tuple l -> TSTuple (List.map (fun x -> infer ctx x) l)
    | True | False -> TSBase "bool"
  in
  typ

(** Infers the type of a match expression.
 A few things happen here.
     - 1. We need to figure out the types of all the
         bound variables in `main`, and ensure there are no duplicates.
     - 2. We need to figure out the type of `main`,
         and make sure that it's valid.
     - 3. We need to make sure that each match pattern has
         the type of `main`.
     - 4. We need to make sure all of the `pats` have the
         same type
          * this should be a fairly trivial fold over unification.
*)
and infer_match ctx main pats =
  let main_typ = infer ctx main in
  let throw_t _ctx _id typ =
    match typ with None -> raise @@ TypeErr "MPId no type" | Some x -> x
  in
  let unwrap t = t.args in
  let rec frees_type pat typ =
    match pat with
    | MPWild -> []
    | MPInt _ -> []
    | MPId t -> [ (t, throw_t ctx t typ) ]
    | MPApp (p, t) ->
        let typ' = unwrap @@ lookup_constr ctx p in
        List.concat @@ List.map2 (fun x y -> frees_type x (Some y)) t typ'
    | MPTup t -> (
        match typ with
        | Some (TSTuple t') ->
            List.concat @@ List.map2 (fun x y -> frees_type x (Some y)) t t'
        | _ -> raise @@ TypeErr "Can't pattern match tuple pattern on non-tuple"
        )
  in
  let rec pat_to_type mctx pat =
    match pat with
    | MPWild -> TSMeta (get_meta ())
    | MPInt _ -> TSBase "int"
    | MPId _ -> TSMeta (get_meta ())
    | MPApp (p, _f) ->
        let m = lookup_constr mctx p in
        let t =
          match m.typ with
          | Error () -> raise @@ Impossible "Error(()) adt_pattern typechecking"
          | Ok t -> t
        in
        t
    | MPTup t -> TSTuple (List.map (pat_to_type mctx) t)
  in
  let typs =
    List.map
      (fun (p, e) ->
        let ty = pat_to_type ctx p in
        let frees = frees_type p (Some main_typ) in
        let ctx' =
          List.fold_left (fun c (x, y) -> assume_typ c x y) ctx frees
        in
        let inf = infer ctx' e in
        let bounds =
          match (ty, main_typ) with
          | TSApp (q, _), TSApp (w, _) -> List.map2 (fun x y -> (x, y)) q w
          | _, _ -> []
        in
        (inf, bounds))
      pats
  in
  typs

(** Infers the type of a term

    w.r.t function inference:
    
       alright, so this mess. Basically what this does is
       infer instation of a function - ie, it takes
    {[
       f x
       and turns it into
       f [typeof x] x
     ]}
       but it's not always that simple
    
       does this by inserting metavariables into all the foralls,
       then unifying the LHS of the function with the argument,
       then applying that to the RHS.

           this is basically just normal sysF with unification? kinda?

*)
and infer ctx tm =
  (*
    infer the type of a term
   *)
  let res =
    match tm with
    | Base (inf, x) -> (inf, infer_base ctx x)
    | FCall (inf, f, x) -> (
        let typ_r = infer ctx f in
        let inst_r = inst_all typ_r in
        let typ_l = infer ctx x in
        let inst_l = inst_all typ_l in
        match inst_r with
        | TSMap (a, b) ->
            let res = unify (empty_unify_ctx ()) a inst_l in
            (inf, apply_unify (fst res) b)
        | tp ->
            raise
              (TypeErr
                 ("Cannot apply \n" ^ show_kexpr x ^ " to \n" ^ show_kexpr f
                ^ "\n of type: " ^ pshow_typesig tp)))
    (*
    try and infer/check one side against the other and vice versa
   *)
    | IfElse (inf, c, e1, e2) -> (
        ignore (check ctx c (TSBase "bool"));
        try
          let typ = infer ctx e1 in
          ignore (check ctx e2 typ);
          (inf, typ)
        with TypeErr _ ->
          let typ = infer ctx e2 in
          ignore (check ctx e1 typ);
          (inf, typ))
    | LetIn (inf, id, e1, e2) ->
        let bodytyp = infer ctx e1 in
        let intyp = infer (assume_typ ctx id bodytyp) e2 in
        (inf, intyp)
    | LetRecIn (inf, ts, id, e1, e2) ->
        let ctx' = assume_typ ctx id ts in
        ignore (check ctx' e1 ts);
        let intyp = infer (assume_typ ctx id ts) e2 in
        (inf, intyp)
    | Join (inf, a, b) ->
        ignore (check ctx a tsBottom);
        (inf, infer ctx b)
    | Inst (_, _, _) -> raise (TypeErr "UNREACHABLE")
    | TypeLam (inf, t, b) ->
        let ctx' = add_bound_typ ctx t in
        let ctx' = add_bound_forall ctx' t (TSMeta (get_meta ())) in
        let bodytyp = infer ctx' b in
        (inf, TSForall (t, bodytyp))
    | TupAccess (inf, expr, i) -> (
        match infer ctx expr with
        | TSTuple t ->
            print_int @@ List.length t;
            if List.length t < i then
              raise @@ TypeErr "Tuple access of too-small tuple"
            else
              (inf, List.nth t i)
        | _ ->
            raise
              (TypeErr ("can't tuple access non-tuple:\n" ^ show_kexpr expr)))
    | AnnotLet (inf, id, ts, e1, e2) ->
        ignore (check ctx e1 ts);
        let ctx' = assume_typ ctx id ts in
        (inf, infer ctx' e2)
    | AnnotLam (inf, id, ts, e) ->
        let out = infer (assume_typ ctx id ts) e in
        (inf, TSMap (ts, out))
    | ModAccess (_inf, _path, _id) ->
        raise @@ Impossible "modules in typechecking expr"
    | Match (inf, main, pats) -> (
        let typs = infer_match ctx main pats in
        try
          ( inf,
            ListHelpers.fold_left'
              (fun x y -> snd @@ unify (empty_unify_ctx ()) x y)
              (List.map fst typs) )
        with UnifyErr _e ->
          raise
          @@ TypeErr
               ("Cannot infer match:\n" ^ show_kexpr tm
              ^ "\n Maybe due to GADTs?"))
    | _ ->
        raise
          (TypeErr
             ("Cannot infer:\n" ^ show_kexpr tm ^ "\nMaybe add annotations?"))
  in
  let inf = fst res in
  let ts = validate_typ ctx.types @@ snd res in
  let res' = remove_aliases ctx.aliases @@ elim_unused @@ lift_ts ts in
  Hash.add_typ inf.id res';
  res'

(** Checks a type against a term *)
and check ctx tm tp =
  (*
    check a type against a term
   *)
  let tp = remove_aliases ctx.aliases @@ elim_unused tp in
  let inf =
    match (tm, tp) with
    | Lam (inf, id, bd), TSMap (a, b) ->
        ignore (check (assume_typ ctx id a) bd b);
        Some inf
    | AnnotLam (inf, id, ts, bd), TSMap (a, b) ->
        ignore (unify (empty_unify_ctx ()) a ts);
        ignore (check (assume_typ ctx id a) bd b);
        Some inf
    | TypeLam (inf, a, b), TSForall (fv, bd) ->
        let typ' = subs bd fv (TSBase a) in
        let ctx' = add_bound_typ ctx a in
        let ctx' = add_bound_forall ctx' fv (TSMeta (get_meta ())) in
        ignore (check ctx' b typ');
        Some inf
    | LetIn (inf, id, e1, e2), bd ->
        let bdtyp = infer ctx e1 in
        ignore (check (assume_typ ctx id bdtyp) e2 bd);
        Some inf
    | AnnotLet (inf, id, ts, e1, e2), bd ->
        let bdtyp = infer ctx e1 in
        ignore (unify (empty_unify_ctx ()) ts bdtyp);
        ignore (check (assume_typ ctx id bdtyp) e2 bd);
        Some inf
    | Base (inf, Tuple x), TSTuple ts ->
        List.iter2 (fun x y -> ignore (check ctx x y)) x ts;
        Some inf
    | Match (inf, main, pats), bd ->
        let typs = infer_match ctx main pats in
        List.iter
          (fun (ty, binds) ->
            let fixed =
              List.fold_left (fun x (t1, t2) -> subs_bad x t2 t1) bd binds
            in
            ignore @@ unify (empty_unify_ctx ()) fixed ty)
          typs;
        Some inf
    | term, exp ->
        let actual = infer ctx term in
        ignore (unify (empty_unify_ctx ()) actual exp);
        None
  in
  match inf with Some s -> Hash.add_typ s.id tp | None -> ()

(** See conv_args_body_to_typelams *)
let rec forall_to_typelam ts body =
  match ts with
  | TSForall (fv, bd) ->
      let tmp = forall_to_typelam bd body in
      (fst tmp, TypeLam (dummyinfo, fv, snd tmp))
  | _ -> (lift_ts ts, body)

(** See conv_args_body_to_typelams *)
let rec add_args ts args body =
  match (ts, args) with
  | TSMap (a, b), x :: xs ->
      AnnotLam (mkinfo (), x, elim_unused a, add_args b xs body)
  | _, [ x ] -> AnnotLam (mkinfo (), x, elim_unused ts, body)
  | _, [] -> body
  | _, _ ->
      raise
        (TypeErr
           ("Cannot match args: " ^ String.concat ", " args ^ " with typesig "
          ^ pshow_typesig ts))

(**
            The purpose of this is to transform arguments into
            typelams and annotlams so that you can do
   {[
            sig ∀a b, a -> (a -> b) -> b in
            let apply x f = f x

            =>

            sig ∀a b, a -> (a -> b) -> b in
            let apply =
            ΛΑ =>
            ΛΒ =>
            λx : A =>
            λf : A -> B =>
            f x
            ]}
   this is currently *really hacky* and also depends on the order
   of the typevars if there aren't enough arguments to the function,
   so uh
   maybe refactor?
   
           *)
let conv_ts_args_body_to_typelams ts args body =
  let ts = elim_unused ts in
  let type_without_foralls = fst @@ forall_to_typelam ts body in
  let args_fixed = add_args type_without_foralls args body in
  snd @@ forall_to_typelam ts args_fixed

let type_simpl ctx t = t |> lift_ts |> remove_aliases ctx.aliases

(** Typecheck a list of toplevel elems *)
let rec typecheck_toplevel_list ctx tl =
  match tl with
  | [] -> ctx
  | x :: xs ->
      let ctx' =
        match x with
        | TopAssign ((id, ts), (_id, args, body)) ->
            let ts = type_simpl ctx ts in

            let body = conv_ts_args_body_to_typelams ts args body in

            if id = "main" then
              ignore
              @@ unify (empty_unify_ctx ()) ts (TSMap (TSTuple [], TSTuple []));

            let fixed = body in
            check ctx fixed ts;
            assume_typ ctx id ts
        | TopAssignRec ((id, ts), (_id, args, body)) ->
            let ts = type_simpl ctx ts in
            let body = conv_ts_args_body_to_typelams ts args body in

            if id = "main" then
              ignore
              @@ unify (empty_unify_ctx ()) ts (TSMap (TSTuple [], TSTuple []));
            let fixed = body in
            (*
              to check a recursive,            
              we assume the type is correct within the expr first
            *)
            let ctx' = assume_typ ctx id ts in
            check ctx' fixed ts;
            ctx'
        (*
         assume the type is correct
        *)
        | Extern (id, _arity, ts) ->
            let ts = type_simpl ctx ts in

            assume_typ ctx id ts
        | IntExtern (_nm, id, _arity, ts) ->
            let ts = type_simpl ctx ts in

            assume_typ ctx id ts
        | Open _ | SimplModule (_, _) ->
            raise @@ Impossible "Modules in typechecking"
        | Bind (id, _, ed) ->
            let typ = lookup ctx ed in
            let ts = type_simpl ctx typ in
            assume_typ ctx id ts
        | Typealias (id, args, ts) ->
            let ts = type_simpl ctx ts in
            let ctx' = add_alias ctx id args ts in
            if List.length args = 0 then
              add_bound_typ ctx' id
            else
              add_param_typ ctx' (List.length args) id
        | Typedecl (id, args, pats) ->
            let rec go xs p =
              match xs with
              | [] -> p
              | [ x ] -> TSMap (x, p)
              | x :: xs -> TSMap (x, go xs p)
            in
            let ctx' =
              List.fold_left
                (fun ctx pat ->
                  match pat.typ with
                  | Error () -> raise @@ Impossible "pat.typ Error"
                  | Ok t ->
                      (match t with
                      | TSApp (_, nm) ->
                          if nm <> id then
                            raise
                            @@ TypeErr
                                 ("GADT constructor " ^ pat.head ^ " in " ^ id
                                ^ " does not have valid return type (Must be \
                                   application to " ^ id ^ ", is instead to "
                                ^ nm ^ ")")
                          else
                            ()
                      | x ->
                          raise
                          @@ TypeErr
                               ("GADT constructor " ^ pat.head ^ " in " ^ id
                              ^ " does not have valid return type (Must be \
                                 application to " ^ id ^ ", is instead "
                              ^ pshow_typesig x ^ ")"));
                      let ts = go pat.args t in
                      let ts' =
                        List.fold_left
                          (fun acc x -> subs acc x (TSMeta (get_meta ())))
                          ts args
                        |> inst_all
                      in
                      assume_typ ctx pat.head ts')
                ctx pats
            in
            let pats' =
              List.map
                (fun x ->
                  {
                    head = x.head;
                    args =
                      List.map
                        (fun y ->
                          List.fold_left
                            (fun acc x -> subs acc x (TSMeta (get_meta ())))
                            y args)
                        x.args;
                    typ = x.typ;
                  })
                pats
            in
            let ctx' = add_constrs ctx' pats' in
            if List.length args = 0 then
              add_bound_typ ctx' id
            else
              add_param_typ ctx' (List.length args) id
      in
      typecheck_toplevel_list ctx' xs

(** Typecheck program *)
let typecheck_program p ctx =
  match p with Program tl -> typecheck_toplevel_list ctx tl

(** Typecheck program list *)
let rec typecheck_program_list_h pl ctx =
  let ctx' = match ctx with Some x -> x | None -> empty_typ_ctx () in
  match pl with
  | [] -> ()
  | x :: xs ->
      let ctx'' = typecheck_program x ctx' in
      typecheck_program_list_h xs (Some ctx'')

(** Helper *)
let typecheck_program_list pl = typecheck_program_list_h pl None

let%test "Typechecking general" =
  let tm =
    Program(
      [
        TopAssign(("test",TSBase("int")),("test",[],
                                          Base(dummyinfo, Int("5"))
                                         )) 
      ]
    )
  in
  try
    typecheck_program_list [tm];
    true
  with
  | _ -> false
