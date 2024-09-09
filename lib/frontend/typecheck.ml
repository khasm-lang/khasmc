open Share.Uuid
open Ast
open Share.Result
open Share.Maybe
open Unify


type ctx = {
  (* name, parent *)
  ctors : (resolved * resolved typdef) list;
  types : resolved typdef list;
  traitfuns : (resolved * resolved trait) list;
  funs : (resolved * (resolved, no) definition) list;
  locals : (resolved * resolved typ) list;
  local_polys : resolved list;
}

let empty_ctx () =
  {
    ctors = [];
    types = [];
    traitfuns = [];
    funs = [];
    locals = [];
    local_polys = [];
  }

let add_local ctx a t = { ctx with locals = (a, t) :: ctx.locals }
let add_locals ctx t = { ctx with locals = t @ ctx.locals }

exception Case
exception DoneTy of resolved typ

let case' (type t) v f =
  match v with Some s -> raise @@ DoneTy (f s) | None -> ()

let search (ctx : ctx) (id : resolved) : (resolved typ, string) result
    =
  try
    case' (List.assoc_opt id ctx.locals) (fun t -> t);

    case' (List.assoc_opt id ctx.ctors) (fun t ->
        match t.content with
        | Record _ -> failwith "shouldn't be variable-ing a record"
        | Sum s -> typdef_and_ctor_to_typ t id);

    case' (List.assoc_opt id ctx.funs) (fun d -> definition_type d);

    case' (List.assoc_opt id ctx.traitfuns) (fun t ->
        let d =
          List.find
            (fun (d : ('a, 'b) definition) -> d.name = id)
            t.functions
        in
        definition_type d);

    failwith "variable not found"
  with DoneTy s -> ok s

let type_information : resolved typ by_uuid = new_by_uuid 100
let add_type uuid typ = Hashtbl.replace type_information uuid typ

let rec break_down_case_pattern (ctx : ctx) (c : resolved case)
    (t : resolved typ) :
    ((resolved * resolved typ) list, string) result =
  let break_and_map a b =
    List.map2 (break_down_case_pattern ctx) a b
    |> collect
    |> Result.map List.flatten
    |> Result.map_error (String.concat " ")
  in
  match c with
  | CaseVar v -> ok [ (v, t) ]
  | CaseTuple tu -> begin
      match t with
      | TyTuple t' ->
          (* find all subpatterns *)
          break_and_map tu t'
      | _ -> err "not tuple but should be tuple :("
    end
  | CaseCtor (name, args) -> begin
      match t with
      (* TODO: this has a bunch of "assertions" in it, mostly around
         the fact that it assumes that type arguments are properly
         filled in with the righ tnumber of them and whatnot
         - that can obviously be false, so fix that
         - ie don't use `combine` and `find` mostly
      *)
      | TyCustom (head, targs) ->
          (* find ty*)
          let ty =
            List.find (fun (x : 'a typdef) -> x.name = head) ctx.types
          in
          begin
            match ty.content with
            | Record _ -> failwith "shouldn't be a record (fun)"
            | Sum s ->
                (* find constructor *)
                let ctor = List.find (fun x -> fst x = name) s in
                let map = List.combine ty.args targs in
                (* fill in all the type arguments *)
                let inst = List.map (instantiate map) (snd ctor) in
                break_and_map args inst
          end
      | _ -> err "not custom but should be"
    end

let test () = print_endline "nah!"

let rec infer (ctx : ctx) (e : resolved expr) :
    (resolved typ, string) result =
  let* ty =
    match e with
    (* try find that thing *)
    | Var (i, v) -> search ctx v
    | Int (_, _) -> ok TyInt
    | String (_, _) -> ok TyString
    | Char (_, _) -> ok TyChar
    | Float (_, _) -> ok TyFloat
    | Bool (_, _) -> ok TyBool
    | LetIn (i, case, annot, head, body) ->
        (* if there's an annot, check, else infer *)
        let* head'ty =
          match annot with
          | Some ty -> check ctx head ty
          | None -> infer ctx head
        in
        (* get everything out *)
        let* vars = break_down_case_pattern ctx case head'ty in
        let ctx' = add_locals ctx vars in
        infer ctx' body
    | Seq (_, a, b) ->
        (* we know the first branch must be unit *)
        let* _ = check ctx a (TyTuple []) in
        infer ctx b
    | Funccall (_, a, b) ->
        let* a'ty = infer ctx a in
        (* unify the calling type and the argument type to make sure
           they're actually compatible
         *)
        begin
          match a'ty with
          | TyArrow (q, w) ->
             let* b'ty = infer ctx b in
             let* _ = unify ctx.local_polys b'ty q in
             ok w
          | _ -> err "must function call on function type"
        end
    | Binop (_, v) -> search ctx v
    | Lambda (_, v, typ, body) ->
       (* if we don't have a static type we can
          make a meta in order to try and infer the body
          gotta remember to "close up" the meta once we're
          done though, nonlocal inference is sucky
        *)
       let typ = begin match typ with
                 | Some ty -> ty
                 | None -> TyMeta (ref Unresolved)
                 end in
       let ctx = add_local ctx v typ in
       let* body'ty = infer ctx body in
       let* _ = match typ with
         | TyMeta m ->
            (* ocaml ref patterns when *)
          begin match !m with
          | Unresolved -> err "meta remained unsolved"
          | _ -> ok ()
          end
       | _ -> ok ()
       in
       ok @@ TyArrow(typ, body'ty)
    | Tuple (_, ts) ->
       (* i love fp *)
       List.map (infer ctx) ts
       |> collect
       |> Result.map_error (String.concat " ")
       |> Result.map (fun x -> TyTuple x)
    | Annot (_, x, t) ->
       (* switch directions *)
       check ctx x t
    | Match (_, scrut, cases) ->
       (* match is uniquely annoying because of that big old
          fold down at the bottom. this makes error propogation
          a pain, because you want to keep all of those possible
          erroring unifies, but you also don't want the whole thing
          to be a mess
        *)
       let* scrut_typ = infer ctx scrut in
       let handle_case (case: 'a case * 'a expr): ('a typ, string) result =
         let case, expr = case in
         let* vars = break_down_case_pattern ctx case scrut_typ in
         let ctx = add_locals ctx vars in
         infer ctx expr
       in
       let* typs = List.map handle_case cases
                   |> collect
                   |> Result.map_error (String.concat " ")
       in
       begin match typs with
       (* an empty match can return anything, because it can never be
          matched on
        *)
       | [] -> ok @@ TyMeta (ref Unresolved)
       | x :: xs ->
          (* TODO: don't ignore errors here *)
          List.fold_left (fun a b -> unify' ctx.local_polys a b; a) x xs
          |> ok
       end
    | Project (_, x, i) ->
       let* x'ty = infer ctx x in
       begin match x'ty with
       | TyCustom (nm, args) ->
          let typ = List.find (fun (x: 'a typdef) -> x.name = nm) ctx.types in
          begin match typ.content with
          | Record fields ->
             (* we have to consider the case in which the record is
                parameterized therefore, while we know the field we
                are working with, we need to up type arguments and
                perform an instantiation
           *)
             let map = List.combine typ.args args in
             let Field(_, typ) = List.nth fields i in
             ok @@ instantiate map typ
          | Sum _ -> err "should be record not sum"
          end
       | _ -> err "can't be record and not record"
       end 
    | Ref (_, f) ->
       let* t = infer ctx f in
       ok @@ TyRef t
    | Modify (_, old, new') ->
       let* t = search ctx old in
       let* _ = check ctx new' t in
       ok @@ TyTuple []
    | Record (_, nm, fields) ->
       (* this case is mildly annoying, because we have to deal
          with instantiation more explicitly
        *)
       failwith "records"
  in
  let uuid = get_uuid e in
  add_type uuid ty;
  ok (force ty)

and check (ctx : ctx) (e : resolved expr) (t : resolved typ) :
    (resolved typ, string) result =
  (* here, we only consider the cases where checking something
     would actually benefit typechecking as a whole - therefore,
     there's a whole bunch of stuff that we just defer straight back
     to infer
     i think there's technically some trickery that can be done
     with regards to Funccall, but it's late and i can't think of it
     right now
     TODO: look at that later
   *)
  let* ty = match e with
    | LetIn (_, case, ty, head, body) ->
       let* head'ty = match ty with
         | Some t -> check ctx head t
         | None -> infer ctx head
       in
       let* vars = break_down_case_pattern ctx case head'ty in
       let ctx = add_locals ctx vars in
       check ctx body t
    | Seq (_, a, b) ->
       let* _ = check ctx a (TyTuple []) in
       check ctx b t
    | Lambda (_, v, ty, body) ->
       begin match t with
       | TyArrow(q, w) ->
          let* ty = begin match ty with
                   (* logically if we're checking
                      (fun (x: t) -> ...)
                      against
                      q -> w
                      then t == q
                    *)
                   | Some t -> unify ctx.local_polys q t
                   | None -> ok q
                   end
          in
          let ctx = add_local ctx v ty in
          check ctx body w
       | _ -> err "lambda cannot be non-function type"
       end
    | Tuple (_, ts) ->
       begin match t with
       | TyTuple s ->
          if List.length ts <> List.length s then
            err "uneq tuple lengths"
          else
            (* i love fp round 2 *)
            List.map2 (check ctx) ts s
            |> collect
            |> Result.map_error (String.concat " ")
            |> Result.map (fun x -> TyTuple x) 
       | _ -> err "must be tuple type"
       end
    | Match (_, scrut, cases) ->
       (* see comments in infer
          should probably factor all this out tbh
        *)
       let* scrut_typ = infer ctx scrut in
       let handle_case (case: 'a case * 'a expr): ('a typ, string) result =
         let case, expr = case in
         let* vars = break_down_case_pattern ctx case scrut_typ in
         let ctx = add_locals ctx vars in
         (* only difference is that we can check this time *)
         check ctx expr t
       in
       let* typs = List.map handle_case cases
                   |> collect
                   |> Result.map_error (String.concat " ")
       in
       begin match typs with
       (* an empty match can return anything, because it can never be
          matched on
        *)
       | [] -> ok @@ TyMeta (ref Unresolved)
       | x :: xs ->
          (* TODO: don't ignore errors here *)
          List.fold_left (fun a b -> unify' ctx.local_polys a b; a) x xs
          |> ok
       end
    | Ref (_, r) ->
       begin match t with
       | TyRef r't ->
          check ctx r r't
       | _ -> err "cannot check ref against not ref"
       end
    | _ ->
       (* in the general case, we defer to infer (hehe) and then
          come back here and "check our work" with unify
        *)
       let* ty = infer ctx e in
       let* _ = unify ctx.local_polys ty t in
       ok ty
  in
  let uuid = get_uuid e in
  add_type uuid ty;
  ok (force ty)

let typecheck_definition (ctx : ctx) (d : (resolved, yes) definition)
    : (unit, string) result =
  let polys = d.typeargs in
  let args = d.args in
  let self = (d.name, forget_body d) in
  let ctx =
    {
      ctx with
      locals = ctx.locals @ args;
      local_polys = ctx.local_polys @ polys;
      (* yay recursion *)
      funs = self :: ctx.funs;
    }
  in
  let body = get d.body in
  let* _ = check ctx body d.return in
  ok ()

let typecheck_impl (ctx: ctx) (i: resolved impl): (unit, string) result =
  (* TODO: check that args match the trait *)
  List.map (typecheck_definition ctx) i.impls
  |> collect
  |> Result.map_error (String.concat " ")
  |> Result.map (fun _ -> ())

let typecheck_toplevel (ctx: ctx) (t: resolved toplevel):
      (unit, string) result =
  match t with
  | Typdef _ -> ok ()
  | Trait _ -> ok ()
  | Impl i ->
     typecheck_impl ctx i
  | Definition d ->
     typecheck_definition ctx d

let gather (t : resolved toplevel list) : ctx =
  let ctx = empty_ctx () in
  List.fold_left
    (fun ctx a ->
      match a with
      | Typdef t -> begin
          match t.content with
          | Record r ->
              {
                ctx with
                ctors = (t.name, t) :: ctx.ctors;
                types = t :: ctx.types;
              }
          | Sum s ->
              List.fold_left
                (fun acc a ->
                  { acc with ctors = (fst a, t) :: acc.ctors })
                { ctx with types = t :: ctx.types }
                s
        end
      | Trait t ->
          List.fold_left
            (fun acc (a : ('a, 'b) definition) ->
              { acc with traitfuns = (a.name, t) :: acc.traitfuns })
            ctx t.functions
      | Impl _ ->
         (* we don't do anything here *)
          ctx
      | Definition d ->
          { ctx with funs = (d.name, forget_body d) :: ctx.funs })
    ctx t

let typecheck_toplevel (t : resolved toplevel list) : unit =
  let ctx = gather t in

