open Frontend.Ast
open Frontend.Typecheck
open Share.Maybe
open Share.Uuid

type m_name = resolved
type m_typ = resolved typ
type m_expr = resolved expr

type monomorph_info = {
  pre_monomorph : (m_name, (m_name, yes) definition) Hashtbl.t;
  monomorph_information :
    (m_name * m_typ, (m_name, yes) definition) Hashtbl.t;
}

let print_monomorph_info ctx =
  print_endline "\n====== MONOMORPH INFO ======\n";
  Hashtbl.iter
    (fun (nm, ty) def ->
      print_string (show_resolved nm ^ " : " ^ show_typ pp_resolved ty);
      print_string " =\n ";
      print_endline (show_definition pp_resolved pp_yes def))
    ctx.monomorph_information

let new_ctx () =
  {
    pre_monomorph = Hashtbl.create 100;
    monomorph_information = Hashtbl.create 100;
  }

let add_pre_monomorph top ctx =
  top
  |> List.iter (function
       | Definition def -> Hashtbl.add ctx.pre_monomorph def.name def
       | _ -> ())

let rec monomorph_e (ctx : monomorph_info) (map : 'a) (body : m_expr)
    : m_expr =
  let go = monomorph_e ctx map in
  (* First check what the type is *)
  let[@warning "-8"] (Some typ) =
    uuid_by_orig type_information (get_uuid body)
  in
  let new_typ = subst_polys map typ in
  Hashtbl.add type_information (get_uuid body) new_typ;
  match body with
  | MLocal _ | MGlobal _ ->
      (* theoretically impossible, but harmless *)
      failwith "uhh recursion?"
  | Int _ | String _ | Char _ | Float _ | Bool _ -> body
  | Var (d, a) ->
      (* then see if it's something we need to monomorphize *)
      begin
        match Hashtbl.find_opt ctx.pre_monomorph a with
        | None ->
            (* does not need it, simply replace type and move on *)
            MLocal (d, a)
        | Some s ->
            let uuid = monomorph_get ctx s new_typ in
            MGlobal (d, uuid)
      end
  | Funccall (d, f, x) -> Funccall (d, go f, go x)
  | LetIn (d, c, ty, e1, e2) -> LetIn (d, c, ty, go e1, go e2)
  | Seq (d, a, b) -> Seq (d, go a, go b)
  | Binop (i, b, a, c) -> Binop (i, b, go a, go c)
  | Lambda (i, nm, t, e) -> Lambda (i, nm, t, go e)
  | Tuple (i, s) -> Tuple (i, List.map go s)
  | Annot (i, e, t) ->
      (* We remove annotations*)
      go e
  | Match (i, e, cs) ->
      Match (i, go e, List.map (fun (a, b) -> (a, go b)) cs)
  | Project (i, e, k) -> Project (i, go e, k)
  | Ref (i, e) -> Ref (i, go e)
  | Modify (i, a, e) -> Modify (i, a, go e)
  | Record (i, a, cs) ->
      Record (i, a, List.map (fun (a, b) -> (a, go b)) cs)

and monomorph (ctx : monomorph_info) (def : (_, _) definition) monotyp
    : (_, yes) definition =
  let body = get def.body in
  def.data.counter <- def.data.counter + 1;
  let full_typ =
    typ_list_to_typ (List.map snd def.args @ [ def.return ])
  in
  let pairwise = match_polys full_typ monotyp in
  (*print_endline ("monomorphing " ^ show_resolved def.name);
  List.iter (fun (a,b) -> 
      print_endline (show_resolved a);
      print_endline ("  " ^ show_typ pp_resolved b);
    ) pairwise;
   *)
  let incr'd_body = expr_set_uuid_version def.data.counter body in
  let bod = monomorph_e ctx pairwise incr'd_body in
  {
    def with
    data =
      {
        def.data with
        uuid = uuid_set_version def.data.counter def.data.uuid;
      };
    body = Just bod;
    args =
      List.map (fun (a, b) -> (a, subst_polys pairwise b)) def.args;
    return = subst_polys pairwise def.return;
    (* TODO: change bounds? *)
  }

and monomorph_get (ctx : monomorph_info) (def : (_, _) definition) typ
    : uuid =
  match
    Hashtbl.find_opt ctx.monomorph_information (def.name, typ)
  with
  | Some res -> res.data.uuid
  | None -> (
      let def' = monomorph ctx def typ in
      try
        Hashtbl.add ctx.monomorph_information (def'.name, typ) def';
        def'.data.uuid
      with Stack_overflow ->
        failwith
          "you probably tried to recursively monomorphize something")

let monomorphize (top : resolved toplevel list) : monomorph_info =
  let ctx = new_ctx () in
  add_pre_monomorph top ctx;
  (* add everything to the queue *)
  (* TODO: Make this more advanced *)
  let main =
    match Hashtbl.find_opt ctx.pre_monomorph (R "main") with
    | Some def -> def
    | None -> failwith "main not found >:("
  in
  let _ = monomorph_get ctx main (TyArrow (TyInt, TyInt)) in
  ctx
