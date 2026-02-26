open Share.Uuid
open Parsing.Ast
open Share.Result
open Share.Maybe
open Share.Types
open Unify

let typ_pp t = print_endline (show_typ pp_resolved t)

(*
  ASSUMPTIONS:
  this module makes a number of assumptions about the data being
  passed into it. these being violated will cause Issues.

  - all pieces of the same information have the same id
  - all pieces of unrelated information have seperate ids
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

module Locals = Map.Make (CompareResolved)

type ctx = {
  (* name, parent *)
  ctors : (resolved, resolved typdef) Hashtbl.t;
  types : (resolved, resolved typdef) Hashtbl.t;
  funs : (resolved, (resolved, unit, no) definition) Hashtbl.t;
  (* measurable perf bonus *)
  locals : resolved typ Locals.t;
  (* if someone ever has enough polyvars in scope to make this an issue,
     i will be very scared
  *)
  local_polys : resolved list;
}

(*
[@@deriving show { with_path = false }]
*)
let empty_ctx () =
  {
    ctors = Hashtbl.create 100;
    types = Hashtbl.create 100;
    funs = Hashtbl.create 100;
    (* magic, for testing *)
    locals = Locals.empty;
    local_polys = [];
  }

let add_local ctx a t =
  { ctx with locals = Locals.add a t ctx.locals }

let add_locals ctx t =
  {
    ctx with
    locals =
      Locals.union (fun _ a _ -> Some a) (Locals.of_list t) ctx.locals;
  }

exception Case
exception DoneTy of resolved typ

let case' (type t) v f =
  match v with Some s -> raise @@ DoneTy (f s) | None -> ()

let search (ctx : ctx) (id : resolved) : (resolved typ, string) result
    =
  try
    case' (Locals.find_opt id ctx.locals) (fun t -> t);

    case' (Hashtbl.find_opt ctx.ctors id) (fun t ->
        match t.content with
        | Record _ -> failwith "shouldn't be variable-ing a record"
        | Sum s -> typdef_and_ctor_to_typ t id);

    case' (Hashtbl.find_opt ctx.funs id) (fun d -> definition_type d);

    begin
      print_endline "variable not found:";
      print_endline (show_resolved id);
      failwith "oops"
    end
  with DoneTy s -> ok s

let type_information : (unit uuid, resolved typ) Hashtbl.t =
  new_by_uuid 100

let add_type uuid typ = Hashtbl.replace type_information uuid typ

let add_type_with_existing old_uuid =
  let fresh = uuid () in
  let typ = Hashtbl.find type_information old_uuid in
  add_type fresh typ;
  fresh

let ident_type_info : (resolved, resolved typ) Hashtbl.t =
  Hashtbl.create 100

let add_raw_type id typ =
  Hashtbl.replace ident_type_info id (force typ)

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
  | CaseWild -> ok []
  | CaseLit p -> ok []
  | CaseVar v -> ok [ (v, t) ]
  | CaseTuple tu -> begin
      match t with
      | TyTuple t' -> begin
          let a = List.length tu in
          let b = List.length t' in
          if a <> b then
            err
              ("tuple lengths unequal, v vs t is "
              ^ Printf.sprintf "%d vs %d" a b)
          else
            (* find all subpatterns *)
            break_and_map tu t'
        end
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
          begin match Hashtbl.find_opt ctx.types head with
          | None -> err "can't find type"
          | Some ty -> begin
              match ty.content with
              | Record _ -> failwith "shouldn't be a record (fun?)"
              | Sum s ->
                  (* find constructor *)
                  begin match
                    List.find_opt (fun x -> fst x = name) s
                  with
                  | None -> err "can't find ctor"
                  | Some ctor ->
                      let map = List.combine ty.args targs in
                      (* fill in all the type arguments *)
                      let inst =
                        List.map (instantiate map) (snd ctor)
                      in
                      break_and_map args inst
                  end
            end
          end
      | _ -> err "Tried to pattern match on non-pattern matchable"
    end

let rec infer (ctx : ctx) (e : _ expr) : (resolved typ, string) result
    =
  let* ty =
    match e with
    | Fail _ -> failwith "fail node in typechecking"
    | MGlobal _ -> failwith "monomorphization info in typechecking"
    (* try find that thing *)
    | Var (i, v) ->
        let* found = search ctx v in
        (* instantiate stuff now, to assist meta propogation *)
        to_metas ctx.local_polys found |> ok
    | Constructor (i, v) ->
        let* found = search ctx v in
        to_metas ctx.local_polys found |> ok
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
        (* add the relevant type information to the raw type database *)
        ignore (List.map (fun (a, b) -> add_raw_type a b) vars);
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
        begin match a'ty with
        | TyArrow (q, w) ->
            let* b'ty = infer ctx b in
            let* _ = unify ctx.local_polys b'ty q in
            ok w
        | _ -> err "must function call on function type"
        end
    | BinOp (_, op, a, b) -> begin
        match op with
        | Add | Sub | Mul | Div | Lt | LtEq | Gt | GtEq ->
            let* t =
              match check ctx a TyInt with
              | Ok _ -> ok TyInt
              | Error _ -> check ctx a TyFloat
            in
            let* _ = check ctx b t in
            ok t
        | LAnd | LOr ->
            let* _ = check ctx a TyBool in
            let* _ = check ctx b TyBool in
            ok TyBool
        | Eq ->
            let* ty = infer ctx a in
            let* _ = check ctx b ty in
            ok TyBool
      end
    | Lambda (_, v, typ, body) ->
        (* if we don't have a static type we can
          make a meta in order to try and infer the body
          gotta remember to "close up" the meta once we're
          done though, nonlocal inference is sucky
        *)
        let typ =
          begin match typ with
          | Some ty -> ty
          | None -> TyMeta (ref Unresolved)
          end
        in
        let ctx = add_local ctx v typ in
        let* body'ty = infer ctx body in
        let* _ =
          match typ with
          | TyMeta m ->
              (* ocaml ref patterns when *)
              begin match !m with
              (* shouldn't this return a polymorphic type? *)
              | Unresolved -> err "meta remained unsolved"
              | _ -> ok ()
              end
          | _ -> ok ()
        in
        add_raw_type v (force typ);
        ok @@ TyArrow (typ, body'ty)
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
        let handle_case (case : 'a case * ('a, 'b) expr) :
            ('a typ, string) result =
          let case, expr = case in
          let* vars = break_down_case_pattern ctx case scrut_typ in
          (* add raw type info for each *)
          ignore (List.map (fun (a, b) -> add_raw_type a b) vars);
          let ctx = add_locals ctx vars in
          infer ctx expr
        in
        let* typs =
          List.map handle_case cases
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
            List.fold_left
              (fun a b ->
                unify' ctx.local_polys a b;
                a)
              x xs
            |> ok
        end
    | Modify (_, old, new') ->
        let* t = search ctx old in
        let* _ = check ctx new' t in
        ok @@ TyTuple []
    | UnaryOp (_, op, expr) -> begin
        match op with
        | BNegate ->
            let* t = check ctx expr TyBool in
            ok @@ TyBool
        | Negate ->
            let* _ = check ctx expr TyInt in
            ok @@ TyInt
        | Ref ->
            let* t = infer ctx expr in
            ok @@ TyRef t
        | GetRecField r ->
            failwith "implement record field access typechecking"
        | GetConstrField i ->
            failwith "get constr field in typechecking"
        | Project i ->
            let* x'ty = infer ctx expr in
            begin match x'ty with
            | TyCustom (nm, args) ->
                let typ = Hashtbl.find ctx.types nm in
                begin match typ.content with
                | Record fields ->
                    (* we have to consider the case in which the record is
                       parameterized therefore, while we know the field we
                       are working with, we need to up type arguments and
                       perform an instantiation
                     *)
                    let map = List.combine typ.args args in
                    let _, typ = List.nth fields i in
                    ok @@ instantiate map typ
                | Sum _ -> err "should be record not sum"
                end
            | _ -> err "can't be record and not record"
            end
      end
    | Record (_, nm, fields) ->
        (* this case is mildly annoying, because we have to deal
          with instantiation more explicitly
        *)
        (* a bit TODO *)
        let typ = Hashtbl.find ctx.types nm in
        begin match typ.content with
        | Record r ->
            (* make sure that the lists contain each other
             TODO: make more efficient
           *)
            if
              not
              @@ (List.for_all
                    (fun (name, value) ->
                      match List.assoc_opt name r with
                      | Some _ -> true
                      | None -> false)
                    fields
                 && List.for_all
                      (fun (name, typ) ->
                        match List.assoc_opt name fields with
                        | Some _ -> true
                        | None -> false)
                      r)
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
              List.iter
                (fun res ->
                  let match' = List.assoc (fst res) r in
                  unify' ctx.local_polys (snd res) match')
                res;
              ok @@ TyCustom (typ.name, List.map snd metas)
        | _ -> err "can't make a record out of a sum type"
        end
  in
  let uuid = get_uuid e in
  add_type uuid ty;
  ok (force ty)

and check (ctx : ctx) (e : (resolved, 'b) expr) (t : resolved typ) :
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
  (*
  print_endline ("checking: " ^ show_expr pp_resolved e);
  print_endline ("  against: " ^ show_typ pp_resolved t);
   *)
  let* ty =
    match e with
    | LetIn (_, case, ty, head, body) ->
        let* head'ty =
          match ty with
          | Some t -> check ctx head t
          | None -> infer ctx head
        in
        let* vars = break_down_case_pattern ctx case head'ty in
        ignore (List.map (fun (a, b) -> add_raw_type a b) vars);
        let ctx = add_locals ctx vars in
        check ctx body t
    | Seq (_, a, b) ->
        let* _ = check ctx a (TyTuple []) in
        check ctx b t
    | Lambda (_, v, ty, body) -> begin
        match t with
        | TyArrow (q, w) ->
            let* ty =
              begin match ty with
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
            add_raw_type v ty;
            let ctx = add_local ctx v ty in
            check ctx body w
        | _ -> err "lambda cannot be non-function type"
      end
    | Tuple (_, ts) -> begin
        match t with
        | TyTuple s ->
            if List.length ts <> List.length s then begin
              let a = List.length ts in
              let b = List.length s in
              err
                ("typechecking unequal tuple lengths (v "
                ^ string_of_int a
                ^ " ) vs (ty "
                ^ string_of_int b
                ^ ")")
            end
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
        let handle_case (case : 'a case * ('a, 'b) expr) :
            ('a typ, string) result =
          let case, expr = case in
          let* vars = break_down_case_pattern ctx case scrut_typ in
          ignore (List.map (fun (a, b) -> add_raw_type a b) vars);
          let ctx = add_locals ctx vars in
          (* only difference is that we can check this time *)
          check ctx expr t
        in
        let* typs =
          List.map handle_case cases
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
            List.fold_left
              (fun a b ->
                unify' ctx.local_polys a b;
                a)
              x xs
            |> ok
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

let typecheck_definition (ctx : ctx)
    (d : (resolved, 'a, yes) definition) : (unit, string) result =
  let polys = d.typeargs in
  let args = d.args in
  ignore (List.map (fun (a, b) -> add_raw_type a b) args);
  let ctx =
    {
      ctx with
      locals =
        Locals.union
          (fun _ a _ -> Some a)
          ctx.locals (Locals.of_list args);
      local_polys = ctx.local_polys @ polys;
    }
  in
  let body = get d.body in
  let* _ = check ctx body d.return in
  ok ()

let typecheck_toplevel (ctx : ctx)
    (t : (resolved, unit, void) toplevel) : (unit, string) result =
  match t with
  | Typdef _ -> ok ()
  | Definition d ->
      add_raw_type d.name (definition_type d);
      typecheck_definition ctx d

let gather (t : (resolved, 'a, void) toplevel list) : ctx =
  let ctx = empty_ctx () in
  List.fold_left
    (fun ctx (a : ('b, 'a, void) toplevel) ->
      match a with
      | Typdef t -> begin
          match t.content with
          | Record r ->
              Hashtbl.add ctx.ctors t.name t;
              Hashtbl.add ctx.types t.name t;
              ctx
          | Sum s ->
              List.iter (fun a -> Hashtbl.add ctx.ctors (fst a) t) s;
              Hashtbl.add ctx.types t.name t;
              ctx
        end
      | Definition d ->
          Hashtbl.add ctx.funs d.name (forget_body d);
          ctx)
    ctx t

let typecheck (t : (resolved, 'a, void) toplevel list) : unit =
  let ctx = gather t in
  List.map (typecheck_toplevel ctx) t |> collect |> function
  | Ok _ ->
      (*
       TODO: is this needed?
make sure metas don't escape (iter hashtbl?) *)
      let gen = fresh_resolved in
      Hashtbl.iter
        (fun k v ->
          Hashtbl.replace type_information k (back_to_polys gen v))
        type_information;
      ()
  | Error e ->
      List.iter print_endline e;
      failwith "typechecking failed :despair:"
