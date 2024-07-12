open Ast
open Common.Info
open Common.Error
module BUR = BatUref
open Unify
open Common

type ('a, 'b) either = ('a, 'b) Either.t
type info += Type
type data += Type' of ty
type info += IsTrait
type data += IsTrait'

let is_trait id = set_property id IsTrait IsTrait'
let set_ty id ty = set_property id Type (Type' ty)
let get_ty id = get_property id Type

let print_types () =
  print_related_entries Type (fun [@warning "-8"] id (Type' t) ->
      print_string (show_id id);
      print_string " : ";
      print_ty (force t))

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
  {
    bound = [ ("MAGIC", id' (), ([ "a" ], Free "a")) ];
    types = [];
    frees = [];
    locals = [];
    constrs = [];
  }

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

let rec find_frees (ty : ty) =
  match force ty with
  | Free s -> [ (s, "fresh_free_" ^ string_of_int @@ Fresh.fresh ()) ]
  | Tuple t -> List.flatten @@ List.map find_frees t
  | Arrow (a, b) -> find_frees a @ find_frees b
  | TApp (_, b) -> List.flatten @@ List.map find_frees b
  | TForall (_, s) -> find_frees s
  | _ -> []

let rec rename_frees' map ty =
  match force ty with
  | Free s -> Free (List.assoc s map)
  | Tuple t -> Tuple (List.map (rename_frees' map) t)
  | Arrow (a, b) -> Arrow (rename_frees' map a, rename_frees' map b)
  | TApp (t, b) -> TApp (t, List.map (rename_frees' map) b)
  | TForall (s, b) -> TForall (s, rename_frees' map b)
  | _ -> ty

let rename_frees ty =
  let map = find_frees ty in
  rename_frees' map ty

let find_ty (ctx : ctx) (p : string) : (ty, 'a) result =
  match List.filter (fun (nm, _, _) -> nm = p) ctx.locals with
  | [ (_, _, ty) ] -> ok (snd ty)
  | _ -> (
      match List.filter (fun (nm, _, _) -> nm = p) ctx.bound with
      (* TODO: THIS IS VERY IMPORTANT !!! *)
      (* HAVING CLASHING FREE VARIABLES IS NOT GOOD *)
      | [ (_, _, ty) ] -> ok (rename_frees (snd ty))
      | _ -> (
          match
            List.filter (fun (nm, _, _, _) -> nm = p) ctx.constrs
          with
          | [ (_, _, _, ty) ] -> ok (rename_frees @@ snd ty)
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

let find_typ_by_record_nm (ctx : ctx) (nm : string) :
    (typ * (string * ty) list, 'a) result =
  match List.filter (fun (t : typ) -> t.name = nm) ctx.types with
  | [ x ] -> begin
      match x.expr with
      | TRecord l -> ok (x, l)
      | _ -> err @@ `Type_Not_Record (ctx, nm)
    end
  | _ -> err @@ `Impossible_ctx (ctx, "can't find record: " ^ nm)

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
  | Definition def ->
      let ty = mk_ty (List.map snd def.args) def.ret in
      add_def ctx def.name def.id (def.free_vars, ty)
  | Type typ -> add_typ ctx typ
  | Trait trait -> process_trait ctx trait.id trait
  | Impl _ -> ctx (* we ignore impls for the moment *)

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
        (* TODO: let generalize ? *)
        let* head't = infer_tm ctx head in
        let* vars = deduce_pat_type ctx pat head't in
        let ctx' =
          List.map (fun (nm, ty) -> (nm, id, ty)) vars
          |> add_locals ctx
        in
        check_tm ctx' body t
    | Match (id, head, body), t ->
        let* head't = infer_tm ctx head in
        (* TODO: check *)
        let go pat =
          List.fold_right
            (fun (pat, body) acc ->
              let* _ = acc in
              let* vars = deduce_pat_type ctx pat head't in
              let ctx' =
                List.map (fun (nm, ty) -> (nm, id, ty)) vars
                |> add_locals ctx
              in
              check_tm ctx' body t)
            pat (ok ())
        in
        go body
    | Lam (_, pat, tymb, body), Arrow (a, b) ->
        let* _ =
          begin
            match tymb with
            | None -> ok ()
            | Some t -> unify ctx.frees t a |$> fun _ -> ()
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
        and+ _ = unify ctx.frees ty t in
        ()
    | Record (_, nm, fields), Custom t ->
        let* typ, field't = find_typ_by_record_nm ctx (to_str nm) in
        if nm <> t then
          err @@ `Record_Type_Mismatch (ctx, nm, t)
        else
          let fields =
            List.sort
              (fun (nm, _) (nm2, _) -> String.compare nm nm2)
              fields
          in
          let field't =
            List.sort
              (fun (nm, _) (nm2, _) -> String.compare nm nm2)
              field't
          in
          begin
            match List.combine fields field't with
            | exception Invalid_argument _ ->
                (* TODO: better errors here *)
                err @@ `Mismatched_Record (ctx, fields, field't)
            | comb ->
                List.map
                  (fun ((nm1, arg), (nm2, ty)) ->
                    if nm1 <> nm2 then
                      err @@ `Mismatched_Field_Ty (ctx, nm1, nm2)
                    else
                      check_tm ctx arg ty)
                  comb
                |> collect
                |$> fun _ -> ()
          end
    | tm, ty ->
        let* tm't = infer_tm ctx tm in
        unify ctx.frees tm't ty
  end
  |$> fun () -> set_ty (get_tm_id tm) ty

and infer_tm (ctx : ctx) (tm : tm) : (ty, 'a) result =
  let open Common in
  begin
    match tm with
    | App (id, f, x) ->
        let* x't = collect @@ List.map (infer_tm ctx) x in
        let rec go ty xs =
          match (ty, xs) with
          | Arrow (a, b), x :: xs ->
              let* _ = unify ctx.frees a x in
              go b xs
          | t, [] -> ok t
          | _ -> err @@ `App_Mismatch (id, f, x)
        in
        let* f't = infer_tm ctx f in
        let f't' = inst_frees ctx.frees f't in
        go f't' x't
    | Var (_, t) -> find_ty ctx t
    | Bound (_, t) -> find_ty ctx (to_str t)
    | Bool _ -> ok TyBool
    | String _ -> ok TyString
    | Int _ -> ok TyInt
    | Char _ -> ok TyChar
    | Let (id, pat, head, body) ->
        (* TODO: let generalization ? *)
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
    | Project (_, tm, field) ->
        let* tm't = infer_tm ctx tm in
        begin
          match tm't with
          | Custom r ->
              let* typ, fields =
                find_typ_by_record_nm ctx (to_str r)
              in
              begin
                match List.assoc_opt field fields with
                | Some n -> ok n
                | None -> err @@ `Field_Not_Found (ctx, field)
              end
          | _ -> err @@ `Project_Not_Record (ctx, tm't)
        end
    | Annot (id, tm, ty) ->
        let+ _ = check_tm ctx tm ty in
        ty
    (* TODO: add a lot more cases here *)
    | Match (id, tm, exprs) ->
        let* tm't = infer_tm ctx tm in
        let rec go (pat, body) =
          let* vars = deduce_pat_type ctx pat tm't in
          let ctx' =
            List.map (fun (nm, ty) -> (nm, id, ty)) vars
            |> add_locals ctx
          in
          infer_tm ctx' body
        in
        List.map go exprs |> collect |=> fun tms ->
        let rec go acc = function
          | [] -> ok acc
          | x :: xs ->
              let* rest = go acc xs in
              unify ctx.frees x rest |$> fun () -> x
        in
        begin
          match tms with
          | [] -> err @@ `Can't_Infer_Empty_Match (ctx, tm)
          | x :: xs -> go x xs
        end
    | _ ->
        print_endline (show_ctx ctx);
        print_endline (show_tm tm);
        failwith "infer"
  end
  |$> fun ty ->
  set_ty (get_tm_id tm) ty;
  ty

let typecheck_statement (traits : trait list) (ctx : ctx)
    (s : statement) : (statement, 'a) result =
  match s with
  | Definition def ->
      let ctx' = add_frees ctx def.free_vars in
      let ctx' = add_args ctx' def.id def.args in
      (* TODO: check all toplevel types are valid *)
      let+ _ = check_tm ctx' def.body def.ret in
      Definition def
  | Impl impl ->
      let rec subst_free (ty : ty) : ty =
        match force (ty : ty) with
        | Free s -> begin
            match List.assoc_opt s impl.args with
            | Some n -> n
            | None -> Free s
          end
        | Tuple t -> Tuple (List.map subst_free t)
        | Arrow (a, b) -> Arrow (subst_free a, subst_free b)
        | TApp (a, b) -> TApp (a, List.map subst_free b)
        | TForall (a, b) -> TForall (a, subst_free b)
        | _ -> ty
      in
      (* TODO: this is not correct in the general case *)
      let related_trait =
        List.find (fun (t : trait) -> t.name = impl.name) traits
      in
      let funs =
        List.sort
          (fun (t1 : definition) t2 -> String.compare t1.name t2.name)
          impl.impls
      in
      let tfuns =
        List.sort
          (fun (t1 : definition_no_body) t2 ->
            String.compare t1.name t2.name)
          related_trait.functions
      in
      List.map2
        (fun (fn : definition) (tfn : definition_no_body) ->
          let args =
            List.map2
              (fun (nm, _) (_, ty) -> (nm, subst_free ty))
              fn.args tfn.args
          in
          let ret = subst_free tfn.ret in
          let ctx' = add_frees ctx tfn.free_vars in
          let ctx' = add_args ctx' fn.id args in
          check_tm ctx' fn.body ret)
        funs tfuns
      |> collect
      |$> fun _ -> Impl impl
  | _ -> ok s

let typecheck (files : statement list) : (statement list, 'a) result =
  let open Common in
  let ctx = List.fold_left collect_statement (empty ()) files in
  let traits =
    List.filter (function Trait _ -> true | _ -> false) files
    |> List.map (function
         | Trait t -> t
         | _ -> Log.fatal "shouldn't be non traits")
  in
  match
    collect @@ List.map (typecheck_statement traits ctx) files
  with
  | Ok s -> ok s
  | Error e ->
      List.iter
        (fun e ->
          match e with
          | `App_Mismatch (id, tm, tms) -> Log.error "appmismatch"
          | `Bad_Pattern (pat, ty) ->
              Log.trace (show_pat pat);
              Log.trace (show_ty ty);
              Log.error "bad pat"
          | `Bad_Unify ((t1, t1'), (t2, t2')) ->
              Log.trace "bad unify";
              Log.trace (show_ty t1');
              Log.trace (show_ty t2');
              Log.trace "as part of:";
              Log.trace (show_ty t1);
              Log.trace (show_ty t2);
              Log.error "bad unify oop"
          | `Constr_Not_In_Type (pat, ty) ->
              Log.error "constr not in typ"
          | `Impossible i ->
              Log.trace i;
              Log.error "impossible"
          | `Lam_Not_Arrow (tm, ty) -> Log.error "lam not arrow"
          | `Mismatch_Pattern_Args (pat, ty) ->
              Log.error "mismatch pat arg"
          | `Mismatched_Tuple_Length (pat, ty) ->
              Log.error "tuple len mismatch"
          | `Impossible_ctx (ctx, i) ->
              Log.trace (show_ctx ctx);
              Log.trace i;
              Log.error "impossible"
          | `Mismatched_Field_Ty (ctx, s, e) ->
              Log.trace (show_ctx ctx);
              Log.trace s;
              Log.trace e;
              Log.error "mismatched field type"
          | `Mismatched_Record (ctx, fields, tys) ->
              Log.trace (show_ctx ctx);
              List.iter
                (fun (nm, _) -> Log.trace ("field: " ^ nm))
                fields;
              List.iter
                (fun (nm, ty) ->
                  Log.trace ("field: " ^ nm ^ " : " ^ show_ty ty))
                tys;
              Log.error "mismatched record"
          | `Record_Type_Mismatch (ctx, exp, got) ->
              Log.trace (show_ctx ctx);
              Log.trace (to_str exp);
              Log.trace (to_str got);
              Log.error "record type mismatch"
          | `Type_Not_Record (ctx, s) ->
              Log.trace (show_ctx ctx);
              Log.trace s;
              Log.error "type not record"
          | `Project_Not_Record e -> Log.error "proj not record"
          | `Field_Not_Found e -> Log.error "field not found"
          | `Can't_Infer_Empty_Match e -> Log.error "empty match")
        e;
      err' "Typechecking failed"
