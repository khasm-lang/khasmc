open Ast
open Common.Info
open Common.Error
module BUR = BatUref
open Unify

type ('a, 'b) either = ('a, 'b) Either.t
type info += Type
type data += Type' of ty
type info += IsTrait
type data += IsTrait'

let is_trait id = set_property id IsTrait IsTrait'
let set_ty id ty = set_property id Type (Type' ty)
let get_ty id = get_property id Type

let rec is_rank_one' (t : ty) : bool =
  match force t with
  | Tuple t -> List.for_all is_rank_one' t
  | Arrow (a, b) -> is_rank_one' a && is_rank_one' b
  | TApp (_, l) -> List.for_all is_rank_one' l
  | TForall _ -> false
  | _ -> true

let rec is_rank_two' (t : ty) : bool =
  match force t with
  | Tuple t -> List.for_all is_rank_two' t
  | Arrow (a, b) -> is_rank_two' a && is_rank_two' b
  | TApp (_, l) -> List.for_all is_rank_two' l
  | TForall (_, p) -> is_rank_one' p
  | _ -> true

let is_rank_one (x : ty) : (ty, 'a) result =
  match is_rank_one' x with
  | true -> Ok x
  | false -> err @@ `Is_Not_Rank_One x

let is_rank_two (x : ty) : (ty, 'a) result =
  match is_rank_two' x with
  | true -> Ok x
  | false -> err @@ `Is_Not_Rank_Two x

let match' x cont =
  let+ x = x in
  cont x

type ctx = {
  bound : (string * id * ty') list;
  types : typ list;
  frees : (string * kind) list;
  locals : (string * id * ty') list;
}
[@@deriving show { with_path = false }]

let empty () = { bound = []; types = []; frees = []; locals = [] }

let add_def ctx nm id ty =
  { ctx with bound = (nm, id, ty) :: ctx.bound }

let add_typ ctx t = { ctx with types = t :: ctx.types }

let add_local ctx nm id ty =
  { ctx with locals = (nm, id, ty) :: ctx.locals }

let add_frees ctx frees = { ctx with frees = frees @ ctx.frees }

let add_args ctx id args =
  {
    ctx with
    locals =
      List.map (fun (a, b) -> (a, id, ([], b))) args @ ctx.locals;
  }

(*
   can also return a free variable, hence the either 
*)

let find_kind_by_nm' (ctx : ctx) (p : path) :
    ((typ, string * kind) either, 'a) result =
  match List.filter (fun x -> fst x = to_str p) ctx.frees with
  | [ x ] -> ok @@ Either.right x
  | x :: xs -> err @@ `Impossible "duplicate free var"
  | _ -> (
      match List.filter (fun (x : typ) -> x.name = p) ctx.types with
      | [ x ] -> ok @@ Either.left x
      | _ -> err @@ `Impossible "find_type_by_nm")

let find_kind_by_nm (ctx : ctx) (p : path) : (kind, 'a) result =
  let open Either in
  let+ def = find_kind_by_nm' ctx p in
  match def with Left l -> l.kind | Right r -> snd r

let find_ty (ctx : ctx) (id : id) (p : string) : (ty, 'a) result =
  match List.filter (fun (nm, _, _) -> nm = p) ctx.locals with
  | [ (_, _, ty) ] -> ok (snd ty)
  | [] -> err @@ `Impossible ("var not found " ^ p)
  | x :: xs -> err @@ `Impossible ("duplicate var " ^ p)

(* TODO: check *)
let find_typ_by_constr (ctx : ctx) (nm : string) :
    (typ * ty list, 'a) result =
  match
    List.flatten
    @@ List.map
         (fun typ ->
           match typ.expr with
           (* TODO: Add support for variants *)
           | TVariant l ->
               List.filter (fun (s, l) -> s = nm) l
               |> List.map (fun s -> (typ, s))
           | _ -> [])
         ctx.types
  with
  | [ (typ, (ctornm, tys)) ] -> ok @@ (typ, tys)
  | _ -> err @@ `Impossible "constructor not found"

let process_trait (ctx : ctx) (id : id)
    ({ name; args; assoc_types; constraints; functions } : trait) :
    ctx =
  (* we treat the trait functions as "magic" for the moment,
     giving them the most general type we can
  *)
  let rec go (ctx : ctx) : definition list -> ctx = function
    | [] -> ctx
    | def :: xs ->
        let ctx' =
          add_def ctx (to_str def.name) id
            (def.free_vars, mk_ty (List.map snd def.args) def.ret)
        in
        go ctx' xs
  in
  go ctx functions

let collect_statement (ctx : ctx) (state : statement) : ctx =
  match state with
  | Definition (id, def) ->
      let ty = mk_ty (List.map snd def.args) def.ret in
      add_def ctx (to_str def.name) id (def.free_vars, ty)
  | Type (id, typ) -> add_typ ctx typ
  | Trait (id, trait) -> process_trait ctx id trait
  | Impl (_, _) -> ctx (* we ignore impls for the moment *)

(*
   goal: to take a pattern, a context, and a type, and figure out
   which variables in the pattern have which type (and whether
   the whole thing is well typed, too)
*)
let rec deduce_pat_type (ctx : ctx) (pat : pat) (ty : ty) :
    ((string * ty') list, 'a) result =
  match (pat, ty) with
  | Bind s, ty -> ok @@ ((s, no_frees ty) :: [])
  | PTuple t, Tuple ts ->
      if List.length t <> List.length ts then
        err @@ `Mismatched_Tuple_Length (pat, ty)
      else
        collect @@ List.map2 (deduce_pat_type ctx) t ts
        |> Result.map List.flatten
  | Constr (head, body), TApp (pth, args) ->
      (* the complicated case :P *)
      let* typ, tys = find_typ_by_constr ctx (to_str head) in
      if typ.name <> pth then
        err @@ `Wrong_Constructor_Type (pat, ty)
      else if List.length body <> List.length tys then
        err @@ `Mismatch_Pattern_Args (pat, ty)
      else
        (* typ should be the same as we would get by
           searching manually via pth
        *)
        failwith "handle this csae"
  | _ -> err @@ `Bad_Pattern (pat, ty)

let rec check_tm (ctx : ctx) (tm : tm) (ty : ty) : (unit, 'a) result =
  match (tm, ty) with
  | Let (id, pat, head, body), t ->
      let* head't = infer_tm ctx head in
      let* vars = deduce_pat_type ctx pat head't in
      let ctx' =
        List.fold_left
          (fun acc (nm, ty) -> add_local acc nm id ty)
          ctx vars
      in
      check_tm ctx' body t
  | Match (_, _, _), t -> failwith "match"
  | Lam (_, _, _, _), Arrow (a, b) -> failwith "lam"
  | Lam _, _ -> err @@ `Lam_Not_Arrow (tm, ty)
  | ITE (_, _, _, _), t -> failwith "ite"
  | Annot (_, _, _), t -> failwith "annot"
  | Record (_, _, _), t -> failwith "record"
  | Project (_, _, _), t -> failwith "project"
  | Poison (_, _), t -> failwith "poison"
  | tm, ty ->
      let+ tm't = infer_tm ctx tm in
      ignore @@ unify tm't ty

and infer_tm (ctx : ctx) (tm : tm) : (ty, 'a) result =
  match tm with
  | App (id, f, x) ->
      let* x't = collect @@ List.map (infer_tm ctx) x in
      let rec go ty xs =
        match (ty, xs) with
        | Arrow (a, b), x :: xs ->
            ignore @@ unify a x;
            go b xs
        | t, [] -> ok t
        | _ -> err @@ `App_Mismatch (id, f, x)
      in
      let* f't = infer_tm ctx f in
      go f't x't
  | _ -> failwith "infer"

(* TODO: document nicer *)
let rec do_kind_app p l typ list : (kind, 'a) result =
  match (typ, list) with
  | KArrow (a, b), x :: xs ->
      let* _ = unify_kind a x in
      do_kind_app p l b xs
  | Star, x :: xs -> err @@ `Kind_App_Doesn't_Match (p, l)
  | t, [] -> ok t

let rec check_ty (ctx : ctx) (k : kind) (ty : ty) : (unit, 'a) result
    =
  match (force ty, k) with
  | TyInt, Star | TyBool, Star | TyChar, Star | TyString, Star ->
      ok ()
  | Free a, k ->
      let k' = List.assoc a ctx.frees in
      let+ _ = unify_kind k k' in
      ()
  | Custom p, k ->
      let* kind = find_kind_by_nm ctx p in
      let+ _ = unify_kind kind k in
      ()
  | Tuple t, Star ->
      let+ _ = collect @@ List.map (fun x -> check_ty ctx Star x) t in
      ()
  | Arrow (a, b), Star ->
      let+ _ = check_ty ctx Star a
      and+ _ = check_ty ctx Star b in
      ()
  | TApp (p, l), k ->
      let* kind = find_kind_by_nm ctx p in
      let* args = collect @@ List.map (infer_ty ctx) l in
      let* ret = do_kind_app p l kind args in
      let+ _ = unify_kind ret k in
      ()
  | TForall (l, inner), k ->
      let ctx' = add_frees ctx l in
      (* TODO: I think this is correct, going off ghci? *)
      check_ty ctx' k inner
  | TyMeta m, _ -> begin
      match get m with
      | Solved t -> err @@ `Impossible "force should have handled"
      | Unsolved -> err @@ `Cannot_Check_Unknown_Meta m
    end
  | t, k ->
      let* inf = infer_ty ctx t in
      let+ _ = unify_kind inf k in
      ()

and infer_ty (ctx : ctx) (ty : ty) : (kind, 'a) result =
  match force ty with
  | TyInt | TyBool | TyChar | TyString -> ok Star
  | Free s -> ok @@ List.assoc s ctx.frees
  | Custom p -> find_kind_by_nm ctx p
  | Tuple t ->
      let+ _ = collect @@ List.map (check_ty ctx Star) t in
      Star
  | Arrow (a, b) ->
      let+ _ = check_ty ctx Star a
      and+ _ = check_ty ctx Star b in
      Star
  | TApp (p, tyls) ->
      let* k = find_kind_by_nm ctx p
      and* rest = collect @@ List.map (infer_ty ctx) tyls in
      let+ kind = do_kind_app p tyls k rest in
      kind
  | TForall (fv, t) ->
      let ctx' = add_frees ctx fv in
      infer_ty ctx' t
  | TyMeta m -> begin
      match get m with
      | Solved _ -> err @@ `Impossible "force should have handled"
      | Unsolved -> err @@ `Cannot_Check_Unknown_Meta m
    end

let typecheck_ty (ctx : ctx) (ty : ty) : (unit, 'a) result =
  let* ty = is_rank_two ty in
  check_ty ctx Star ty

let typecheck_statement (ctx : ctx) (s : statement) :
    (statement, 'a) result =
  match s with
  | Definition (id, def) ->
      let ctx' = add_frees ctx def.free_vars in
      let ctx' = add_args ctx' id def.args in
      let+ _ = failwith "check the body!"
      and+ _ =
        collect
        @@ List.map (fun (a, b) -> typecheck_ty ctx' b) def.args
      and+ _ = typecheck_ty ctx' def.ret in
      Definition (id, def)
  | Impl (id, impl) -> failwith "todo: typecheck impl"
  | _ -> ok s

let typecheck (files : statement list) : statement list =
  let ctx = List.fold_left collect_statement (empty ()) files in
  match collect @@ List.map (typecheck_statement ctx) files with
  | Ok s -> s
  | Error e -> failwith "handle errors"
