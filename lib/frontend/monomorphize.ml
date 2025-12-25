open Parsing.Ast
open Typecheck
open Share.Maybe
open Share.Uuid

module TypSet = Set.Make(
                    struct
                      type t = resolved typ
                      let compare = compare
                    end
                  )

type monomorph_ctx = {
    mutable customs_to_be_done : TypSet.t;
    poly_map : (resolved * resolved typ) list;
    locals : resolved list;
  }

let add_custom ctx cus =
  ctx.customs_to_be_done <-
    TypSet.add cus ctx.customs_to_be_done

let update_with_uuid (x : ('a, 'b) expr) (nw : resolved typ)
    : ('a, resolved typ) expr =
  let f d = {
      d with uuid = uuid_set_snd nw d.uuid
    }
  in
  data_transform f x

let new_monomorph_ctx () =
  {
    customs_to_be_done = TypSet.empty;
    poly_map = [];
    locals = [];
  }

let rec monomorph_typ
      (map : (resolved * resolved typ) list)
      (ty : resolved typ)
        : resolved typ =
  let rec go ty =
    match force ty with
    | TyArrow (a,b) -> TyArrow (go a, go b)
    | TyPoly a ->
       begin match List.assoc_opt a map with
       | Some t -> go t
       | None -> failwith "all polys should be accounted for"
       end
    | TyTuple t -> TyTuple (List.map go t)
    | TyCustom (a, args) -> TyCustom (a, List.map go args)
    | TyRef a -> TyRef (go a)
    | ty -> ty
  in
  go ty

let def_with_new_body_typ
      (def : ('a, unit, 'c) definition)
      (expr : ('a, resolved typ) expr)
      (typ : 'a typ)
    : ('a, resolved typ, 'c) definition =
  let (args, ret) = typ_to_args_ret typ in
  { def with
    data = { def.data with uuid = uuid_set_snd typ def.data.uuid };
    args = List.combine
             (List.map fst def.args)
             args;
    return = ret;
    body = Just expr;
  }

let (let$) v cont =
  let (exp, defs) = v in
  let (exp', defs') = cont exp in
  (exp', defs @ defs')

let (let$$) v cont =
  let (exp, defs) = v in
  let (exp') = cont exp in
  (exp', defs)

let rec monomorph_expr (ctx : monomorph_ctx)
          (exp : (resolved, unit) expr)
        : (resolved, resolved typ) expr
          * _ definition list =
  let mono_ty = subst_polys in
  let rec go
            (ctx : monomorph_ctx)
            (expr : (_, unit) expr)
          : (resolved, resolved typ) expr
            * _ definition list =
    let mono_expr_ty expr =
      Hashtbl.find type_information (get_uuid expr)
      |> mono_ty ctx.poly_map
    in
    let mk_data dat =
      update_data_uuid dat (mono_expr_ty expr)
    in
    match expr with
    | Var (data, nm) ->
       if List.mem nm ctx.locals then
         (MLocal (mk_data data, nm), [])
       else begin
           failwith "mono global"
         end 
    | LetIn(data, cases, mbtyp, head, body) ->
       let ctx' =
         { ctx with
           locals = case_names cases @ ctx.locals
         }
       in
       let$ head' = go ctx head in
       let$$ body' = go ctx' body in
       LetIn(mk_data data, cases, None, head', body')
    | Funccall (data, f, x) ->
       let$ f' = go ctx f in
       let$$ x' = go ctx x in 
       Funccall (mk_data data, f', x')
  in
  go ctx exp

and monomorph_def (ctx : monomorph_ctx)
          (def : (resolved, unit, yes) definition)
          (against : resolved typ)
        : (resolved, resolved typ, yes) definition list =
  let combined_typ = definition_type def in
  let mapping = match_polys against combined_typ in
  let ctx' = {
      ctx with
      poly_map = mapping;
      locals = List.map fst def.args;
    } in
  let (exp, rest) = monomorph_expr ctx' (get def.body) in
  let me =
    def_with_new_body_typ def exp against
  in
  me :: rest

let monomorphize (top : (resolved, unit) toplevel list)
    : monomorph_ctx * (resolved, resolved typ) toplevel list =
  let ctx = new_monomorph_ctx () in
  let [@warning "-8"] Definition main = List.find (function
                            | Definition x -> 
                               x.name = R "main"
                            | _ -> false) top
  in
  let defs =
    monomorph_def ctx main (TyArrow (TyInt, TyInt))
  in
  let rest =
    let go = function
      | Typdef t -> [Typdef t]
      | Definition _ -> []
    in
    List.flatten @@ List.map go top
  in
  (ctx, rest @ List.map (fun x -> Definition x) defs)
