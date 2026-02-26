open Share.Uuid
open Share.Maybe
open Share.Types
module P = Parsing.Ast
module IR = IR

type name = IR.name

type poly_ctx = {
  poly_constructors : (name, name P.typdef) Hashtbl.t;
  poly_records : (name, name P.typdef) Hashtbl.t;
}

type ctx = {
  constructor_map : (name * IR.typ, name) Hashtbl.t;
  record_map : (name * IR.typ, name) Hashtbl.t;
}

let rec convert_type (poly_ctx : poly_ctx) (ctx : ctx) (t : _ P.typ) :
    IR.typ =
  let go = convert_type poly_ctx ctx in
  match P.force t with
  | P.TyBottom -> IR.TyBottom
  | P.TyInt -> IR.TyInt
  | P.TyString -> IR.TyString
  | P.TyChar -> IR.TyChar
  | P.TyFloat -> IR.TyFloat
  | P.TyBool -> IR.TyBool
  | P.TyTuple t -> IR.TyTuple (List.map go t)
  | P.TyArrow (a, b) -> IR.TyArrow (go a, go b)
  | P.TyPoly _ -> IR.TyIrrelevant
  | P.TyCustom (name, args) -> IR.TyCustom (name, List.map go args)
  | P.TyRef r -> IR.TyRef (go r)
  | P.TyMeta _ -> failwith "meta in IR"

let rec convert_expr (poly_ctx : poly_ctx) (ctx : ctx)
    (expr : (P.resolved, _ P.typ) P.expr) : IR.expr =
  let go = convert_expr poly_ctx ctx in
  (* TODO: why ignore this?
     we need to make sure we collect all the constructors that
     are monomorphized
  *)
  begin match
    Hashtbl.find_opt ParseLang.Typecheck.type_information
      (Share.Uuid.uuid_forget @@ P.get_uuid expr)
  with
  | Some typ -> ignore @@ convert_type poly_ctx ctx typ
  | None -> ()
  end;
  match expr with
  | P.Fail (d, fail) -> IR.Fail (d, fail)
  | P.Var (d, nm) -> IR.Local (d, nm)
  | P.MGlobal (d, uuid, nm) ->
      (* TODO: check for ctor and mono properly *)
      IR.Global (d, nm)
  | P.Constructor (d, nm) -> IR.Constructor (d, nm)
  | P.Int (d, i) -> IR.Int (d, i)
  | P.String (d, i) -> IR.String (d, i)
  | P.Char (d, i) -> IR.Char (d, i)
  | P.Float (d, f) -> IR.Float (d, f)
  | P.Bool (d, b) -> IR.Bool (d, b)
  | P.LetIn (d, P.CaseVar v, _, head, body) ->
      IR.Let (d, v, go head, go body)
  | P.LetIn _ -> failwith "malformed let in IR"
  | P.Seq (d, a, b) -> IR.Seq (d, go a, go b)
  | P.Funccall (d, f, x) ->
      (* figure out how many deep we can go *)
      let rec inner acc expr =
        match expr with
        | P.Funccall (_, f', arg) -> inner (go arg :: acc) f'
        | innermost -> (go innermost, acc)
      in
      let inner, args = inner [] (P.Funccall (d, f, x)) in
      IR.Funccall (d, inner, args)
  | P.BinOp (d, op, a, b) -> IR.BinOp (d, op, go a, go b)
  | P.UnaryOp (d, op, a) -> IR.UnaryOp (d, op, go a)
  | P.Lambda (d, v, _, body) -> IR.Lambda (d, v, go body)
  | P.Tuple (d, t) -> IR.Tuple (d, List.map go t)
  | P.Annot (_, _, _) -> failwith "annot in IR"
  (* if-let *)
  | P.Match
      (d, head, [ (P.CaseCtor (ctor, []), body); (P.CaseVar _, rest) ])
    ->
      IR.IfLet (d, ctor, go head, go body, go rest)
  (* if-exact *)
  | P.Match (d, head, [ (P.CaseLit l, body); (P.CaseVar _, rest) ]) ->
      failwith "match exact"
  | P.Match _ -> failwith "malformed let IR"
  | P.Modify (d, nm, expr) -> IR.Modify (d, nm, go expr)
  | P.Record (d, nm, fields) ->
      IR.Record (d, nm, List.map (Pair.map_snd go) fields)

let convert_def (poly_ctx : poly_ctx) (ctx : ctx)
    (def : (_, _, yes) P.definition) : _ IR.definition =
  let name = def.name in
  let args =
    List.map (Pair.map_snd (convert_type poly_ctx ctx)) def.args
  in
  let body = convert_expr poly_ctx ctx (get def.body) in
  { IR.name; IR.args; IR.body; has_lambdas = Just () }

let convert_to_flat (tops : (P.resolved, 'b, void) P.toplevel list) :
    yes IR.program =
  let prog =
    { IR.defs = []; IR.records = []; IR.constructors = [] }
  in
  let with_polys =
    {
      poly_constructors = Hashtbl.create 100;
      poly_records = Hashtbl.create 100;
    }
  in
  let () =
    List.iter
      (fun (p : ('a, 'b, void) P.toplevel) ->
        match p with
        | P.Typdef t -> begin
            match t.content with
            | Record r -> Hashtbl.add with_polys.poly_records t.name t
            | Sum s ->
                Hashtbl.add with_polys.poly_constructors t.name t
          end
        | P.Definition _ -> ())
      tops
  in
  let ctx =
    {
      constructor_map = Hashtbl.create 100;
      record_map = Hashtbl.create 100;
    }
  in
  let with_defs =
    List.fold_left
      (fun acc (p : ('a, 'b, void) P.toplevel) ->
        match p with
        | P.Typdef typ ->
            (* handled these earlier *)
            acc
        | P.Definition d ->
            {
              acc with
              IR.defs = convert_def with_polys ctx d :: acc.IR.defs;
            })
      prog tops
  in
  print_endline "generated:";
  print_endline (IR.show_program (fun _ _ -> ()) with_defs);
  failwith "mono ctors properly?"
