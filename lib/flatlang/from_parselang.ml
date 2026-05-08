open Share.Types
open Share.Maybe
module I = IR
module P = Frontend.Ast

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
  | P.TyArrow (a, b) -> I.TyArrow (conv_typ a, conv_typ b)
  | P.TyCustom (nm, t) -> I.TyCustom (nm, List.map conv_typ t)
  | P.TyRef r -> I.TyRef (conv_typ r)
  | P.TyMeta _ -> failwith "unsolved meta"

let rec conv_back_typ (ty : I.typ) : P.resolved P.typ =
  let go = conv_back_typ in
  match ty with
  | I.TyUnknown -> failwith "tyUnknown conv_back_typ"
  | I.TyBase `Bool -> TyBool
  | I.TyBase `Irr -> TyPoly (P.fresh_resolved ())
  | I.TyBase `Float -> TyFloat
  | I.TyBase `Char -> TyChar
  | I.TyBase `String -> TyString
  | I.TyBase `Int -> TyInt
  | I.TyBase `Bottom -> TyBottom
  | I.TyTuple t -> TyTuple (List.map conv_back_typ t)
  | I.TyArrow (a, b) -> TyArrow (go a, go b)
  | I.TyCustom (nm, t) -> TyCustom (nm, List.map go t)
  | I.TyRef r -> TyRef (go r)

let conv_tag (record_field_indexes : (P.resolved, int) Hashtbl.t)
    (e : ('a, 'b) P.expr) : I.tag =
  match e with
  | P.Fail (_, s) -> I.Fail s
  | P.Extern (_, s) -> I.Extern s
  | P.Var (_, nm) -> Named (`Local, nm)
  | P.MGlobal (_, _, nm) -> Named (`Global, nm)
  | P.Constructor (data, nm) ->
    let ctor_type =
      Hashtbl.find Parselang.Monomorphize.constructor_types
        (Share.Uuid.uuid_forget data.uuid)
        |> List.map conv_typ
    in
    Named (`Constructor ctor_type, nm)
  | P.Int (_, i) -> Prim (`Int, i)
  | P.String (_, s) -> Prim (`String, s)
  | P.Char (_, c) -> Prim (`Char, c)
  | P.Float (_, f) -> Prim (`Float, f)
  | P.Bool (_, b) -> Prim (`Bool, string_of_bool b)
  | P.LetIn (_, P.CaseVar nm, _, _, _) -> Let nm
  | P.LetIn (_, _, _, _, _) -> failwith "Malformed let"
  | P.Seq (_, _, _) -> Seq
  | P.Funccall (_, _, _) -> Funccall
  | P.BinOp (_, o, _, _) -> BinOp o
  | P.UnaryOp (data, P.GetRecField nm, _) ->
      failwith "get record field"
  | P.UnaryOp (_, P.Negate, _) -> UnaryOp Negate
  | P.UnaryOp (_, P.BNegate, _) -> UnaryOp BNegate
  | P.UnaryOp (_, P.Ref, _) -> UnaryOp Ref
  | P.UnaryOp (_, P.Project i, _) -> UnaryOp (Project i)
  | P.Lambda (_, nm, Some ty, _) -> Lambda (nm, conv_typ ty)
  | P.Lambda (_, nm, _, _) ->
      (* TODO: hack *)
      print_endline "from_parselang: defaulting to hack";
      let typ = Hashtbl.find Parselang.Typecheck.ident_type_info nm in
      Lambda (nm, conv_typ typ)
  | P.Tuple (_, _) -> Tuple
  | P.Annot (_, _, _) -> failwith "annot after typechecking"
  | P.UnpackConstructor (_, typs, nms, _, _) ->
      Unpack (List.map conv_typ typs, nms)
  | P.Match (_, _, [ (CaseCtor (nm, _), _); (CaseVar _, _) ]) ->
      (* TODO: this method of implicit structure is pretty icky *)
    IfLet nm
  | P.Match (_, _, [ (CaseLit lit, _); (CaseVar _, _)]) ->
    begin match lit with
    | P.LBool b -> I.IfConst (`Bool, string_of_bool b)
    | P.LInt i -> I.IfConst (`Int, i)
    end
  | P.Match _ ->
      print_endline "MALFORMED MATCH:";
      print_endline
        (P.show_expr P.pp_resolved (P.pp_typ P.pp_resolved) e);
      failwith "malformed match"
  | P.Modify (_, nm, _) -> Modify nm
  | P.Record (_, nm, children) -> Tuple

let unknown_types = ref 0

let rec conv_expr (rfi : (P.resolved, int) Hashtbl.t)
    (e : ('a, 'b) P.expr) : I.expr =
  let f = conv_expr rfi in
  let tag = conv_tag rfi e in
  let children =
    match e with
    | P.LetIn (_, _, _, hd, bd) -> [ f hd; f bd ]
    | P.Seq (_, a, b) -> [ f a; f b ]
    | P.Funccall (_, g, y) -> [ f g; f y ]
    | P.BinOp (_, _, l, r) -> [ f l; f r ]
    | P.UnaryOp (_, _, o) -> [ f o ]
    | P.Lambda (_, _, _, bd) -> [ f bd ]
    | P.Tuple (_, t) -> List.map f t
    | P.Match
        (_, head, [ (_, body); (_, rest) ]) ->
      [ f head; f body; f rest ]
    | P.Modify (_, _, e) -> [ f e ]
    | P.Record (_, _, r) -> List.map (fun x -> f (snd x)) r
    | P.UnpackConstructor (_, _, _, a, b) -> [ f a; f b ]
    | _ -> []
  in
  let uuid = Share.Uuid.uuid () in
  let typ =
    match
      Hashtbl.find_opt Parselang.Typecheck.type_information
        (Share.Uuid.uuid_forget (P.get_data e).uuid)
    with
    | None ->
        incr unknown_types;
        I.TyUnknown
    | Some t -> conv_typ t
  in
  Expr ({ uuid; typ }, tag, children)

let rec conv_top
    (top : (P.resolved, P.resolved P.typ, void) P.toplevel list) :
    I.program =
  let record_field_indexes = Hashtbl.create 100 in
  let ctor_mapping = Hashtbl.create 100 in
  (* build both at once *)
  List.iter
    (fun (top : ('a, P.resolved P.typ, void) P.toplevel) ->
      match top with
      | P.Typdef t -> begin
          match t.content with
          | P.Record r ->
              List.iteri
                (fun i (nm, _) ->
                  Hashtbl.add record_field_indexes nm i)
                r
          | P.Sum ctors -> Hashtbl.add ctor_mapping t.name t
        end
      | _ -> ())
    top;
  let res =
    List.fold_left
      (fun (acc : I.program)
           (top : ('a, P.resolved P.typ, void) P.toplevel) ->
        match top with
        | P.Typdef t -> begin
            match t.content with
            | P.Record r -> acc
            | P.Sum s ->
              let f : int -> 'a * 'b -> unit =
                 fun i (nm, _) -> Hashtbl.add acc.constructors nm i
              in
              List.iteri f s;
              acc
          end
        | P.Extern (nm, typ) ->
            Hashtbl.add acc.externs nm (conv_typ typ);
            acc
        | P.Definition d ->
            {
              acc with
              defs =
                {
                  I.name = d.name;
                  args =
                    List.map
                      (fun (nm, ty) -> (nm, conv_typ ty))
                      d.args;
                  returns = conv_typ d.return;
                  body = conv_expr record_field_indexes (get d.body);
                }
                :: acc.defs;
            })
      {
        I.defs = [];
        constructors = Hashtbl.create 100;
        externs = Hashtbl.create 100;
        gen_type_sizes =
          begin
            fun name args ->
              (* TODO: this is really a hack *)
              let t = Hashtbl.find ctor_mapping name in
              let[@warning "-8"] (P.Sum s) = t.content in
              let res =
                List.map
                  (fun (_, inner) ->
                    List.map
                      (fun typ ->
                        conv_typ
                          (P.subst_polys
                             (List.combine t.args
                                (List.map conv_back_typ args))
                             typ))
                      inner)
                  s
              in
              res
          end;
      }
      top
  in
  res
