open Share.Uuid
open Ast
open Share.Result
open Share.Maybe
open Unify

let typ_pp t = print_endline (show_typ pp_resolved t)

(*
  ASSUMPTIONS:
  this module makes a number of assumptions about the data being
  passed into it. these being violated will cause Issues.

  - all pieces of the same information have the same id
  - all pieces of unrelated information have seperate ids

  for example, something like this:

  trait Foo {
  type b;
  fun foo : Self -> b
  }

  fun dothing (type T) {T: Foo} (x: T): T.{Foo}.b = foo x

  must be something like

  trait 0 {
  type 1;
  fun 2 : 0 -> 1
  }

  fun 3 (type 4) {4: 0} (5: 4): 4.{0}.1 = 2 5

 *)

(*
  general TODOs:
  - add meta refinement
  f : ?
  x : int
  f x
  =>
  f : int -> ?
 *)


type ctx = {
  (* name, parent *)
  ctors : (resolved * resolved typdef) list;
  types : resolved typdef list;
  traits: resolved trait list;
  traitfuns : (resolved * resolved trait) list;
  funs : (resolved * (resolved, no) definition) list;
  locals : (resolved * resolved typ) list;
  local_polys : resolved list;
}
[@@deriving show {with_path = false}]

let empty_ctx () =
  {
    ctors = [];
    types = [];
    traits = [];
    traitfuns = [];
    funs = [];
    (* magic, for testing *)
    locals = [(R (-1), TyArrow(TyPoly (R (-2)), TyPoly (R (-3))))];
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
         begin match
           List.find_opt (fun (x : 'a typdef) -> x.name = head) ctx.types
         with
         | None -> err "can't find type"
         | Some ty ->
          begin
            match ty.content with
            | Record _ -> failwith "shouldn't be a record (fun?)"
            | Sum s ->
               (* find constructor *)
               begin match List.find_opt (fun x -> fst x = name) s with
               | None -> err "can't find ctor"
               | Some ctor ->
                let map = List.combine ty.args targs in
                (* fill in all the type arguments *)
                let inst = List.map (instantiate map) (snd ctor) in
                break_and_map args inst
          end end end
      | _ -> err "not custom but should be"
    end

let test () = print_endline "nah!"

let rec infer (ctx : ctx) (e : resolved expr) :
    (resolved typ, string) result =
  let* ty =
    match e with
    (* try find that thing *)
    | Var (i, v) ->
       let* found = search ctx v in
       (* instantiate stuff now, to assist meta propogation *)
       to_metas ctx.local_polys found
       |> ok
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
        (* TODO: let generalization? *)
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
          | TyArrow (q,w) ->
             let* b'ty = infer ctx b in
             let* _ = unify ctx.local_polys b'ty q in
             ok w
          | _ -> err "must function call on function type"
        end
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

          TODO: note that GADT "helping" cannot be done in the
          inference case
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
             let (_, typ) = List.nth fields i in
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
       (* a bit TODO *)
       let typ = List.find (fun (x: 'a typdef) -> x.name = nm) ctx.types in
       begin match typ.content with
       | Record r ->
          (* make sure that the lists contain each other
             TODO: make more efficient
           *)
          if not @@ (List.for_all (fun (name, value) ->
                        match List.assoc_opt name r with
                        | Some _ -> true
                        | None -> false
                      ) fields
                     &&
                       List.for_all (fun (name, typ) ->
                           match List.assoc_opt name fields with
                           | Some _ -> true
                           | None -> false
                         ) r
                    )
          then
            err "record decl does not match type"
          else
            let our_polys = typ.args in
            let metas =
              List.map
                (fun poly -> (poly, TyMeta (ref Unresolved)))
                our_polys
            in
            List.map (fun field -> infer ctx (snd field)) fields
            |> collect
            |> Result.map_error (String.concat " ")
            |> fun results ->
               let* res = results in
               let res = List.combine (List.map fst fields) res in
               List.iter (
                   fun res ->
                   let match' = List.assoc (fst res) r in
                   unify' ctx.local_polys (snd res) match') res;
               ok @@ TyCustom(typ.name, List.map snd metas)
       | _ -> err "can't make a record out of a sum type"
       end
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

          TODO: add GADT "helping" so that something like
          let e : Eq a b -> a -> b = fun eq x ->
          match eq with
          | Refl -> x
          end
          works
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
       print_endline "check";
       typ_pp t;
       typ_pp ty;
       ok ty
  in
  let uuid = get_uuid e in
  add_type uuid ty;
  ok (force ty)

let typecheck_definition (ctx : ctx) (d : (resolved, yes) definition)
    : (unit, string) result =
  let polys = d.typeargs in
  let args = d.args in
  let ctx =
    {
      ctx with
      locals = ctx.locals @ args;
      local_polys = ctx.local_polys @ polys;
    }
  in
  let body = get d.body in
  let* _ = check ctx body d.return in
  ok ()

let typecheck_impl (ctx: ctx) (i: resolved impl): (unit, string) result =
  (* TODO: check that args match the trait *)
  i.impls
  |> List.map snd (* we don't care about the unique name at the moment *)
  |> List.map (typecheck_definition ctx) 
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

let typecheck (t : resolved toplevel list) : unit =
  let ctx = gather t in
  List.map (typecheck_toplevel ctx) t
  |> collect
  |> function
    | Ok _ ->
       print_endline "worked!";
       ()
    | Error e ->
       List.iter (print_endline) e;
       failwith "typechecking failed :despair:"
