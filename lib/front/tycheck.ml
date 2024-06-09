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

type ctx = {
  bound : (string * id * ty') list;
  (* constr name, associated type name,
     actual arguments,
     type *)
  constrs : (string * string * ty list * ty') list;
  types : typ list;
  frees : freevar list;
  locals : (string * id * ty') list;
}
[@@deriving show { with_path = false }]

let empty () =
  { bound = []; types = []; frees = []; locals = []; constrs = [] }

let add_def ctx nm id ty =
  { ctx with bound = (nm, id, ty) :: ctx.bound }

let add_typ ctx t =
  {
    ctx with
    types = t :: ctx.types;
    constrs =
      (match t.expr with
      | TAlias _ -> []
      | TRecord _ -> []
      | TVariant l ->
          List.map
            (fun (name, args) ->
              let final = Custom (Base t.name) in
              let ty = mk_ty args final in
              ( path_append (Base t.name) name,
                t.name,
                args,
                (t.args, ty) ))
            l)
      @ ctx.constrs;
  }

let add_local ctx nm id ty =
  { ctx with locals = (nm, id, ty) :: ctx.locals }

let rec add_locals ctx l =
  match l with
  | [] -> ctx
  | (nm, id, ty) :: xs ->
      let ctx' = add_local ctx nm id ty in
      add_locals ctx' xs

let add_frees ctx frees = { ctx with frees = frees @ ctx.frees }

let add_args ctx id args =
  {
    ctx with
    locals =
      List.map (fun (a, b) -> (a, id, ([], b))) args @ ctx.locals;
  }

let find_ty (ctx : ctx) (p : string) : (ty, 'a) result =
  match List.filter (fun (nm, _, _) -> nm = p) ctx.locals with
  | [ (_, _, ty) ] -> ok (snd ty)
  | _ -> (
      match List.filter (fun (nm, _, _) -> nm = p) ctx.bound with
      | [ (_, _, ty) ] -> ok (snd ty)
      | _ -> (
          match
            List.filter (fun (nm, _, _, _) -> nm = p) ctx.constrs
          with
          | [ (_, _, _, ty) ] -> ok (snd ty)
          | _ -> err @@ `Impossible_ctx (ctx, "var not found: " ^ p)))

(* TODO: check *)
let find_typ_by_constr_variant (ctx : ctx) (nm : string) :
    (typ * ty list, 'a) result =
  match List.filter (fun (nm', _, _, _) -> nm = nm') ctx.constrs with
  | [ (nm, tyname, args, _) ] -> begin
      match
        List.find_opt (fun (typ : typ) -> typ.name = tyname) ctx.types
      with
      | Some s -> ok (s, args)
      | None ->
          err
          @@ `Impossible_ctx (ctx, "can't find constr parent " ^ nm)
    end
  | _ -> err @@ `Impossible_ctx (ctx, "can't find constr " ^ nm)

let process_trait (ctx : ctx) (id : id)
    ({ name; args; assoc_types; constraints; functions } : trait) :
    ctx =
  (* we treat the trait functions as "magic" for the moment,
     giving them the most general type we can
  *)
  let rec go (ctx : ctx) : definition_no_body list -> ctx = function
    | [] -> ctx
    | def :: xs ->
        let ctx' =
          add_def ctx def.name id
            (def.free_vars, mk_ty (List.map snd def.args) def.ret)
        in
        go ctx' xs
  in
  go ctx functions

let collect_statement (ctx : ctx) (state : statement) : ctx =
  match state with
  | Definition (id, def) ->
      let ty = mk_ty (List.map snd def.args) def.ret in
      add_def ctx def.name id (def.free_vars, ty)
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
  | Constr (head, body), t ->
      (* the complicated case for variants :P *)
      let* typ, tys = find_typ_by_constr_variant ctx (to_str head) in
      if List.length body <> List.length tys then
        err @@ `Mismatch_Pattern_Args (pat, ty)
      else
        let+ x =
          collect @@ List.map2 (deduce_pat_type ctx) body tys
        in
        List.flatten x
  | _ -> err @@ `Bad_Pattern (pat, ty)

let rec check_tm (ctx : ctx) (tm : tm) (ty : ty) : (unit, 'a) result =
  begin
    match (tm, ty) with
    | Let (id, pat, head, body), t ->
        let* head't = infer_tm ctx head in
        let* vars = deduce_pat_type ctx pat head't in
        let ctx' =
          List.map (fun (nm, ty) -> (nm, id, ty)) vars
          |> add_locals ctx
        in
        check_tm ctx' body t
    | Match (id, head, body), t ->
        let* head't = infer_tm ctx head in
        let rec go pat =
          match pat with
          | [] -> ok ()
          | (pat, body) :: xs ->
              let* _ = go xs in
              let* vars = deduce_pat_type ctx pat head't in
              let ctx' =
                List.map (fun (nm, ty) -> (nm, id, ty)) vars
                |> add_locals ctx
              in
              check_tm ctx' body t
        in
        go body
    | Lam (_, pat, tymb, body), Arrow (a, b) ->
        let* _ =
          begin
            match tymb with
            | None -> ok ()
            | Some t -> unify t a |$> fun _ -> ()
          end
        in
        let* tys = deduce_pat_type ctx pat a in
        let ctx' =
          List.fold_left
            (fun acc (a, b) -> add_local acc a noid b)
            ctx tys
        in
        check_tm ctx' body b
    | Lam _, _ -> err @@ `Lam_Not_Arrow (tm, ty)
    | Annot (_, tm, ty), t ->
        let+ _ = check_tm ctx tm ty
        and+ _ = unify ty t in
        ()
    | Record (_, _, _), t -> failwith "record"
    | Project (_, _, _), t -> failwith "project"
    | Poison (_, _), t -> failwith "poison"
    | tm, ty ->
        let* tm't = infer_tm ctx tm in
        unify tm't ty
  end
  |$> fun () -> set_ty (get_tm_id tm) ty

and infer_tm (ctx : ctx) (tm : tm) : (ty, 'a) result =
  begin
    match tm with
    | App (id, f, x) ->
        let* x't = collect @@ List.map (infer_tm ctx) x in
        let rec go ty xs =
          match (ty, xs) with
          | Arrow (a, b), x :: xs ->
              let* _ = unify a x in
              go b xs
          | t, [] -> ok t
          | _ -> err @@ `App_Mismatch (id, f, x)
        in
        let* f't = infer_tm ctx f in
        go f't x't
    | Var (_, t) -> find_ty ctx t
    | Bound (_, t) -> find_ty ctx (to_str t)
    | Bool _ -> ok TyBool
    | String _ -> ok TyString
    | Int _ -> ok TyInt
    | Char _ -> ok TyChar
    | Let (id, pat, head, body) ->
        let* h't = infer_tm ctx head in
        let* adds = deduce_pat_type ctx pat h't in
        let ctx' =
          List.map (fun (nm, ty) -> (nm, id, ty)) adds
          |> add_locals ctx
        in
        infer_tm ctx' body
    | ITE (id, i, t, e) ->
        let* _ = check_tm ctx i TyBool in
        begin
          match infer_tm ctx t with
          | Ok t't ->
              let+ _ = check_tm ctx e t't in
              t't
          | Error _ ->
              let* e't = infer_tm ctx e in
              let+ _ = check_tm ctx t e't in
              e't
        end
    | Annot (id, tm, ty) ->
        let+ _ = check_tm ctx tm ty in
        ty
    | _ ->
        print_endline (show_ctx ctx);
        print_endline (show_tm tm);
        failwith "infer"
  end
  |$> fun ty ->
  set_ty (get_tm_id tm) ty;
  ty

let typecheck_statement (ctx : ctx) (s : statement) :
    (statement, 'a) result =
  match s with
  | Definition (id, def) ->
      let ctx' = add_frees ctx def.free_vars in
      let ctx' = add_args ctx' id def.args in
      (* TODO: check all toplevel types are valid *)
      let+ _ = check_tm ctx' def.body def.ret in
      Definition (id, def)
  | Impl (id, impl) -> failwith "todo: typecheck impl"
  | _ -> ok s

let typecheck (files : statement list) : statement list =
  let ctx = List.fold_left collect_statement (empty ()) files in
  match collect @@ List.map (typecheck_statement ctx) files with
  | Ok s -> s
  | Error e ->
      List.map
        (fun e ->
          match e with
          | `App_Mismatch (id, tm, tms) -> failwith "appmismatch"
          | `Bad_Pattern (pat, ty) ->
              print_endline (show_pat pat);
              print_endline (show_ty ty);
              failwith "bad pat"
          | `Bad_Unify ((t1, t1'), (t2, t2')) ->
              print_endline (show_ty t1');
              print_endline (show_ty t2');
              print_endline "as part of:";
              print_endline (show_ty t1);
              print_endline (show_ty t2);
              failwith "bad unify"
          | `Constr_Not_In_Type (pat, ty) ->
              failwith "constr not in typ"
          | `Impossible i ->
              print_endline i;
              failwith "impossible"
          | `Lam_Not_Arrow (tm, ty) -> failwith "lam not arrow"
          | `Mismatch_Pattern_Args (pat, ty) ->
              failwith "mismatch pat arg"
          | `Mismatched_Tuple_Length (pat, ty) ->
              failwith "tuple len mismatch"
          | `Impossible_ctx (ctx, i) ->
              print_endline (show_ctx ctx);
              print_endline i;
              failwith "impossible")
        e
