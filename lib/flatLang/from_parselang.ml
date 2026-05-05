open Share.Types
open Share.Maybe
module I = IR
module P = Parsing.Ast

let rec conv_typ (ty : P.resolved P.typ) : I.typ =
  match P.force ty with
  | TyBottom -> TyBase `Bottom 
  | TyInt -> TyBase `Int 
  | TyString -> TyBase `String 
  | TyChar -> TyBase `Char
  | TyFloat -> TyBase `Float 
  | TyBool -> TyBase `Bool
  | P.TyTuple t -> I.TyTuple (List.map conv_typ t)
  | P.TyPoly nm -> TyBase `Irr
  | P.TyArrow (a,b) -> I.TyArrow (conv_typ a, conv_typ b)
  | P.TyCustom (nm, t) -> I.TyCustom (nm, List.map conv_typ t)
  | P.TyRef r -> I.TyRef (conv_typ r)
  | P.TyMeta _ -> failwith "unsolved meta"

let conv_tag (e : ('a, 'b) P.expr) : I.tag =
  match e with
   | P.Fail (_, s) -> I.Fail s
   | P.Var (_, nm) -> Named (`Local, nm)
   | P.MGlobal (_, _, nm) -> Named (`Global, nm)
   | P.Constructor (data, nm) ->
     let ctor_type =
       Hashtbl.find
         Parselang.Monomorphize.constructor_types
         (Share.Uuid.uuid_forget data.uuid)
       |> List.map conv_typ
     in
     Named (`Constructor ctor_type, nm)
   | P.Int (_, i) -> Prim (`Int, i)
   | P.String (_, s) -> Prim (`String, s)
   | P.Char (_, c) -> Prim (`Char, c)
   | P.Float (_, f) -> Prim (`Float, f)
   | P.Bool (_, b) -> I.Bool b
   | P.LetIn (_, P.CaseVar nm, _, _, _) -> Let nm
   | P.LetIn (_,_,_,_,_) -> failwith "Malformed let"
   | P.Seq (_, _, _) -> Seq
   | P.Funccall (_, _, _) -> Funccall
   | P.BinOp (_, o, _, _) -> BinOp o
   | P.UnaryOp (_, P.GetRecField nm, _) ->
     failwith "get record field"
   | P.UnaryOp (_, u, _) -> UnaryOp u
   | P.Lambda (_, nm, _, _) -> Lambda nm
   | P.Tuple (_, _) -> Tuple
   | P.Annot (_, _, _) -> failwith "annot after typechecking"
   | P.UnpackConstructor (_, typs, nms, _, _) ->
     Unpack (List.map conv_typ typs, nms)
   | P.Match (_, _, [
       (CaseCtor (nm, _), _);
       (CaseVar _, _)
     ]) ->
     (* TODO: this method of implicit structure is pretty icky *)
     IfLet nm
   | P.Match _ ->
     print_endline "MALFORMED MATCH:";
     print_endline (P.show_expr P.pp_resolved (P.pp_typ P.pp_resolved) e);
     failwith "malformed match"
   | P.Modify (_, nm, _) -> Modify nm
   | P.Record (_, nm, children) ->
     I.Record (nm, List.map fst children)

let unknown_types = ref 0

let rec conv_expr (e : ('a, 'b) P.expr) : I.expr =
  let f = conv_expr in
  let tag = conv_tag e in
  let children = match e with
   | P.LetIn (_, _, _, hd, bd) -> [f hd; f bd]
   | P.Seq (_, a, b) -> [f a; f b]
   | P.Funccall (_, g, y) -> [f g; f y]
   | P.BinOp (_, _, l, r) -> [f l; f r]
   | P.UnaryOp (_, _, o) -> [f o]
   | P.Lambda (_, _, _, bd) -> [f bd]
   | P.Tuple (_, t) -> List.map f t
   | P.Match (_, head, [
       (CaseCtor (nm, _), body);
       (CaseVar _, rest)
     ]) -> [f head; f body; f rest]
   | P.Modify (_, _, e) -> [f e]
   | P.Record (_, _, r) -> List.map (fun x -> (f (snd x))) r
   | P.UnpackConstructor (_, _, _, a, b) -> [f a; f b]
   | _ -> []
  in
  let uuid = Share.Uuid.uuid () in
  let typ =
    match Hashtbl.find_opt
            Parselang.Typecheck.type_information
            (Share.Uuid.uuid_forget (P.get_data e).uuid)
    with
    | None ->
      incr unknown_types;
      I.TyUnknown
    | Some t -> conv_typ t
  in
  Expr ({ uuid; typ }, tag, children)
  
let rec conv_top (top : (P.resolved, P.resolved P.typ, void) P.toplevel list) : I.program =
  let res = List.fold_left (fun (acc : I.program) (top : ('a, P.resolved P.typ, void) P.toplevel) ->
      match top with
      | P.Typdef t ->
        begin match t.content with
          | P.Record r -> {
              acc with
              I.records = {I.name = t.name; fields = List.map fst r} :: acc.records
            }
          | P.Sum s ->
            let f : int -> ('a * 'b) -> I.Ctor.constructor =
              fun i (nm, _) -> {I.Ctor.index = i; I.Ctor.name = nm} in
            let all = List.mapi f s in
            {
              acc with
              I.constructors = all @ acc.constructors
            }
          end
      | P.Definition d -> {
        acc with
          defs = {
            I.name = d.name;
            args = List.map (fun (nm, ty) -> nm, conv_typ ty) d.args;
            returns = conv_typ d.return;
            body = conv_expr (get d.body);
          } :: acc.defs
        }
  ) {I.defs = []; records = []; constructors = []} top
  in
  res
