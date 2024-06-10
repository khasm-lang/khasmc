open Ast

let rec collapse (p : path) : path = Base (to_str p)

let rec collapse_ty (t : ty) : ty =
  match (t : ty) with
  | TyString | TyInt | TyBool | TyChar | Free _ -> t
  | TyMeta m -> (
      match BatUref.uget m with
      | Unsolved -> t
      | Solved t -> collapse_ty t)
  | Custom p -> Custom (collapse p)
  | Tuple t -> Tuple (List.map collapse_ty t)
  | Arrow (a, b) -> Arrow (collapse_ty a, collapse_ty b)
  | TApp (p, t) -> TApp (collapse p, List.map collapse_ty t)
  | TForall (s, p) -> TForall (s, collapse_ty p)

let collapse_cons (con : constraint' list) : constraint' list =
  let go ((p, ty) : constraint') =
    (collapse p, List.map collapse_ty ty)
  in
  List.map go con

let rec collapse_pat (pat : pat) : pat =
  match (pat : pat) with
  | Bind _ -> pat
  | PTuple t -> PTuple (List.map collapse_pat t)
  | Constr (p, s) -> Constr (collapse p, List.map collapse_pat s)

let rec collapse_tm (tm : tm) : tm =
  match (tm : tm) with
  | String _ | Bool _ | Int _ | Char _ -> tm
  | Var (_, _) -> tm
  | Bound (i, p) -> Bound (i, collapse p)
  | App (i, a, bs) -> App (i, collapse_tm a, List.map collapse_tm bs)
  | Let (id, pat, hd, bd) ->
      Let (id, collapse_pat pat, collapse_tm hd, collapse_tm bd)
  | Match (id, tm, l) ->
      Match
        ( id,
          collapse_tm tm,
          List.map (fun (a, b) -> (collapse_pat a, collapse_tm b)) l
        )
  | Lam (id, pat, op, bd) ->
      Lam
        ( id,
          collapse_pat pat,
          Option.map collapse_ty op,
          collapse_tm bd )
  | ITE (i, a, b, c) ->
      ITE (i, collapse_tm a, collapse_tm b, collapse_tm c)
  | Annot (i, tm, ty) -> Annot (i, collapse_tm tm, collapse_ty ty)
  | Record (id, pth, flds) ->
      Record
        ( id,
          collapse pth,
          List.map (fun (a, b) -> (a, collapse_tm b)) flds )
  | Project (i, p, s) -> Project (i, collapse_tm p, s)
  | Poison (_, _) -> tm

let rec collapse_tyexpr (tyexpr : tyexpr) : tyexpr =
  match (tyexpr : tyexpr) with
  | TVariant i ->
      TVariant
        (List.map (fun (a, b) -> (a, List.map collapse_ty b)) i)
  | TRecord i ->
      TRecord (List.map (fun (a, b) -> (a, collapse_ty b)) i)
  | TAlias t -> TAlias (collapse_ty t)

let convert' (s : statement) : statement =
  match (s : statement) with
  | Definition (id, { name; free_vars; constraints; args; ret; body })
    ->
      Definition
        ( id,
          {
            name;
            free_vars;
            constraints = collapse_cons constraints;
            args = List.map (fun (a, b) -> (a, collapse_ty b)) args;
            ret = collapse_ty ret;
            body = collapse_tm body;
          } )
  | Type (id, { name; args; expr }) ->
      Type (id, { name; args; expr = collapse_tyexpr expr })
  | Trait (i, { name; args; assoc_types; constraints; functions }) ->
      Trait
        ( i,
          {
            name;
            args;
            assoc_types;
            constraints = collapse_cons constraints;
            functions =
              List.map
                (fun ({ name; free_vars; constraints; args; ret } :
                       definition_no_body) ->
                  {
                    name;
                    free_vars;
                    constraints = collapse_cons constraints;
                    args =
                      List.map (fun (a, b) -> (a, collapse_ty b)) args;
                    ret = collapse_ty ret;
                  })
                functions;
          } )
  | Impl (i, { name; args; assoc_types; impls }) ->
      Impl
        ( i,
          {
            name;
            args = List.map collapse_ty args;
            assoc_types =
              List.map (fun (a, b) -> (a, collapse_ty b)) assoc_types;
            impls =
              List.map
                (fun { name; free_vars; constraints; args; ret; body } ->
                  {
                    name;
                    free_vars;
                    constraints = collapse_cons constraints;
                    args =
                      List.map (fun (a, b) -> (a, collapse_ty b)) args;
                    ret = collapse_ty ret;
                    body = collapse_tm body;
                  })
                impls;
          } )

let convert (s : statement list) : statement list =
  List.map convert' s
