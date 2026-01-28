open Parsing.Ast
open Typecheck
open Share.Maybe
open Share.Types
open Share.Uuid

module ResSet = Set.Make (struct
  type t = resolved

  let compare (R (a, b)) (R (x, y)) = compare a x
end)

type monomorph_ctx = {
  have_done : (resolved typ uuid, unit) Hashtbl.t; [@opaque]
  poly_map : (resolved * resolved typ) list;
  definitions :
    (resolved, (resolved, unit, yes) definition) Hashtbl.t;
      [@opaque]
  locals : ResSet.t; [@opaque]
}
[@@deriving show { with_path = false }]

let update_with_uuid (x : ('a, 'b) expr) (nw : resolved typ) :
    ('a, resolved typ) expr =
  let f d = { d with uuid = uuid_set_snd nw d.uuid } in
  data_transform f x

let new_monomorph_ctx top =
  {
    have_done = Hashtbl.create 100;
    poly_map = [];
    locals = ResSet.empty;
    definitions =
      (let tbl = Hashtbl.create 100 in
       List.iter
         (function
           | Definition d -> Hashtbl.add tbl d.name d | _ -> ())
         top;
       tbl);
  }

let rec monomorph_typ (map : (resolved * resolved typ) list)
    (ty : resolved typ) : resolved typ =
  let rec go ty =
    match force ty with
    | TyArrow (a, b) -> TyArrow (go a, go b)
    | TyPoly a -> begin
        match List.assoc_opt a map with
        | Some t -> go t
        | None -> failwith "all polys should be accounted for"
      end
    | TyTuple t -> TyTuple (List.map go t)
    | TyCustom (a, args) -> TyCustom (a, List.map go args)
    | TyRef a -> TyRef (go a)
    | ty -> ty
  in
  go ty

let def_with_new_body_typ (def : ('a, unit, 'c) definition)
    (expr : ('a, resolved typ) expr) (typ : 'a typ) :
    ('a, resolved typ, 'c) definition =
  let args, ret = typ_to_args_ret typ in
  {
    def with
    data = { def.data with uuid = uuid_set_snd typ def.data.uuid };
    args = List.combine (List.map fst def.args) args;
    return = ret;
    body = Just expr;
  }

let ( let$ ) v cont =
  let exp, defs = v in
  let exp', defs' = cont exp in
  (exp', defs @ defs')

let ( let$$ ) v cont = (cont @@ fst v, snd v)

(*
  let exp, defs = v in
  let exp' = cont exp in
  (exp', defs)
*)

let rec monomorph_expr (ctx : monomorph_ctx)
    (exp : (resolved, unit) expr) :
    (resolved, resolved typ) expr * _ definition list =
  let mono_ty = subst_polys in
  let rec go (ctx : monomorph_ctx) (expr : (_, unit) expr) :
      (resolved, resolved typ) expr * _ definition list =
    let mono_expr_ty expr =
      (* possible perf problem spot? loooots of hashtbl lookups *)
      Hashtbl.find type_information (get_uuid expr)
      |> mono_ty ctx.poly_map
    in
    let data' =
      update_data_uuid (get_data expr) (mono_expr_ty expr)
    in
    match expr with
    | Var (_, nm) ->
        if ResSet.mem nm ctx.locals then
          (Var (data', nm), [])
        else begin
          let def = Hashtbl.find ctx.definitions nm in
          let new_uuid =
            uuid_set_snd (mono_expr_ty expr) def.data.uuid
          in
          let defs =
            if not (Hashtbl.mem ctx.have_done new_uuid) then begin
              Hashtbl.add ctx.have_done new_uuid ();
              monomorph_def ctx def (mono_expr_ty expr)
            end
            else
              []
          in
          (MGlobal (data', new_uuid, nm), defs)
        end
    | LetIn (_, cases, _, head, body) ->
        let ctx' =
          {
            ctx with
            locals =
              ResSet.union
                (ResSet.of_list @@ case_names cases)
                ctx.locals;
          }
        in
        let$ head' = go ctx head in
        let$$ body' = go ctx' body in
        LetIn (data', cases, None, head', body')
    | Seq (_, a, b) ->
        let$ a' = go ctx a in
        let$$ b' = go ctx b in
        Seq (data', a', b')
    | Funccall (_, f, x) ->
        let$ f' = go ctx f in
        let$$ x' = go ctx x in
        Funccall (data', f', x')
    | BinOp (_, bop, l, r) ->
        let$ l' = go ctx l in
        let$$ r' = go ctx r in
        BinOp (data', bop, l', r')
    | Lambda (_, nm, _, body) ->
        let ctx' = { ctx with locals = ResSet.add nm ctx.locals } in
        let$$ body' = go ctx' body in
        Lambda (data', nm, None, body')
    | Tuple (_, tups) ->
        let tups', defs = List.split (List.map (go ctx) tups) in
        (Tuple (data', tups'), List.flatten defs)
    | Annot (_, exp, _) ->
        (* these don't exist anymore, but no harm *)
        failwith "annotations in monomorphization"
    | Match (_, expr, cases) ->
        let$ expr' = go ctx expr in
        let cases', defs =
          List.map
            (fun (case, expr) ->
              let ctx' =
                {
                  ctx with
                  locals =
                    ResSet.union
                      (ResSet.of_list @@ case_names case)
                      ctx.locals;
                }
              in
              let$$ expr' = go ctx' expr in
              (case, expr'))
            cases
          |> List.split
        in
        (Match (data', expr', cases'), List.flatten defs)
    | UnaryOp (_, op, expr) ->
        let$$ expr' = go ctx expr in
        UnaryOp (data', op, expr')
    | Modify (_, nm, expr) ->
        let$$ expr' = go ctx expr in
        Modify (data', nm, expr')
    | Record (_, tynm, cases) ->
        let cases', defs =
          List.split
            (List.map
               (fun (a, b) ->
                 let case, defs = go ctx b in
                 ((a, case), defs))
               cases)
        in
        (Record (data', tynm, cases'), List.flatten defs)
    | _ -> (data_transform (fun _ -> data') expr, [])
  in
  go ctx exp

and monomorph_def (ctx : monomorph_ctx)
    (def : (resolved, unit, yes) definition) (against : resolved typ)
    : (resolved, resolved typ, yes) definition list =
  let combined_typ = definition_type def in
  let mapping = match_polys against combined_typ in
  let ctx' =
    {
      ctx with
      poly_map = mapping;
      locals = ResSet.of_list @@ List.map fst def.args;
    }
  in
  let exp, rest = monomorph_expr ctx' (get def.body) in
  let me = def_with_new_body_typ def exp against in
  me :: rest

let monomorphize (top : (resolved, unit, void) toplevel list) :
    monomorph_ctx * (resolved, resolved typ, void) toplevel list =
  let ctx = new_monomorph_ctx top in
  let[@warning "-8"] (Definition main) =
    List.find
      (function
        | Definition x ->
            let (R (a, b)) = x.name in
            a = "main"
        | _ -> false)
      top
  in
  let defs = monomorph_def ctx main (TyArrow (TyInt, TyInt)) in
  let rest =
    let go : ('a, 'b, void) toplevel -> 'c = function
      | Typdef t -> [ Typdef t ]
      | Definition _ -> []
    in
    List.flatten @@ List.map go top
  in
  (ctx, rest @ List.map (fun x -> Definition x) defs)
