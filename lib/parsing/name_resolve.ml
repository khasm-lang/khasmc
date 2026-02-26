open Ast
open Share.Types
open Share.Uuid
open Share.Maybe

type global_ctx = {
  module_name : string;
  children : global_ctx list;
  mutable parent : global_ctx option; [@opaque]
  globals : (string, resolved) Hashtbl_p.t;
  types : (string, resolved) Hashtbl_p.t;
  constructors : (string, resolved) Hashtbl_p.t;
  record_fields : (string, resolved) Hashtbl_p.t;
}
[@@deriving show { with_path = false }]

let new_global_ctx nm =
  {
    module_name = nm;
    children = [];
    parent = None;
    globals = Hashtbl.create 50;
    types = Hashtbl.create 10;
    constructors = Hashtbl.create 10;
    record_fields = Hashtbl.create 10;
  }

module StrMap = Map.Make (String)

type local_ctx = {
  typevars : resolved StrMap.t;
  locals : resolved StrMap.t;
}

let get_typevar ctx var = StrMap.find_opt var ctx.typevars
let get_local ctx var = StrMap.find_opt var ctx.locals

let add_typevar ctx a b =
  { ctx with typevars = StrMap.add a b ctx.typevars }

let add_local ctx a b =
  { ctx with locals = StrMap.add a b ctx.locals }

let new_local_ctx () =
  { typevars = StrMap.empty; locals = StrMap.empty }

let new_local_ctx' typevars locals = { typevars; locals }

let rec top_ctx ctx =
  match ctx.parent with Some x -> top_ctx x | None -> ctx

let split_on_dot str = String.split_on_char '.' str

let rec get_current_module_name ctx =
  match ctx.parent with
  | None -> ctx.module_name
  | Some parent ->
      let rest = get_current_module_name parent in
      rest ^ "." ^ ctx.module_name

let merge_tables source sink =
  Hashtbl.iter
    (fun k v ->
      match Hashtbl.find_opt sink k with
      | None -> Hashtbl.add sink k v
      | Some _ -> failwith "duplicate name")
    source

let add_tbl_no_duplicates tbl name =
  match Hashtbl.find_opt tbl name with
  | None ->
      if name = "main" then
        Hashtbl.add tbl "main" (R 0)
      else
        let resolved = resolved_using name in
        Hashtbl.add tbl name resolved
  | Some nm -> failwith ("duplicate name: " ^ name)

(*
   this does _NOT_ take into account opened modules, or module aliases
   the former is handled in the gathering of contexts, and the
   latter is a TODO
*)
let resolve_to_ctx (ctx : global_ctx) (name : string) is_module :
    global_ctx * string =
  let split = split_on_dot name in
  match (split, is_module) with
  | [], _ -> failwith "empty name"
  | [ nm ], true ->
      (* either top or "same level" *)
      let top = top_ctx ctx in
      if top.module_name = nm then
        (top, name)
      else begin
        match
          ctx.parent
        with
        | None -> failwith "resolve_to_ctx same level no parent"
        | Some parent -> (
            match
              List.find_opt
                (fun x -> x.module_name = nm)
                parent.children
            with
            | None ->
                failwith
                  ("name "
                  ^ name
                  ^ " does not resolve to valid module")
            | Some ctx -> (ctx, name))
      end
  | path, _ ->
      let rec go ctx path =
        match (path, is_module) with
        | [], true -> (ctx, name)
        | [], false -> failwith ("couldn't find path " ^ name)
        | [ nm ], false -> (ctx, nm)
        | nm :: rest, _ -> (
            match
              List.find_opt (fun x -> x.module_name = nm) ctx.children
            with
            | None ->
                failwith
                  ("couldn't find path "
                  ^ name
                  ^ " (no child "
                  ^ nm
                  ^ ")")
            | Some child -> go child rest)
      in
      go (top_ctx ctx)
        (if is_module then
           List.tl split
         else
           split)

let get_global ctx nm =
  let ctx, small = resolve_to_ctx ctx nm false in
  Hashtbl.find_opt ctx.globals small

let get_type ctx nm =
  let ctx, small = resolve_to_ctx ctx nm false in
  Hashtbl.find_opt ctx.types small

let get_constructor ctx nm =
  let ctx, small = resolve_to_ctx ctx nm false in
  Hashtbl.find_opt ctx.constructors small

let get_record_field ctx nm =
  let ctx, small = resolve_to_ctx ctx nm false in
  Hashtbl.find_opt ctx.record_fields small

let rec construct_global_ctx ctx
    (tops : (string, 'b, unit) toplevel list) : global_ctx =
  let add = add_tbl_no_duplicates in
  let ctx =
    List.fold_left
      (fun acc top ->
        match top with
        | Definition d ->
            add ctx.globals d.name;
            acc
        | Typdef t ->
            add ctx.types t.name;
            begin match t.content with
            | Record r ->
                List.iter (fun (nm, _) -> add ctx.record_fields nm) r
            | Sum s ->
                List.iter (fun (nm, _) -> add ctx.constructors nm) s
            end;
            acc
        | Open name ->
            (* resolve opens later *)
            acc
        | Module (nm, inner) ->
            let inner_ctx = new_global_ctx nm in
            let inner_ctx' = construct_global_ctx inner_ctx inner in
            { acc with children = inner_ctx' :: acc.children })
      ctx tops
  in
  (* we need to fill in children with the _fully constructed_
     final one
  *)
  List.iter (fun child -> child.parent <- Some ctx) ctx.children;
  ctx

let rec resolve_opens ctx (tops : ('a, 'b, unit) toplevel list) =
  List.iter
    (fun top ->
      match top with
      | Module (nm, inner) ->
          let child =
            List.find (fun x -> x.module_name = nm) ctx.children
          in
          resolve_opens child inner
      | Open name ->
          let found = fst @@ resolve_to_ctx ctx name true in
          merge_tables found.globals ctx.globals;
          merge_tables found.constructors ctx.constructors;
          merge_tables found.record_fields ctx.record_fields;
          merge_tables found.types ctx.types
      | _ -> ())
    tops

let rec resolve_type ctx l_ctx (typ : string typ) : resolved typ =
  let go = resolve_type ctx l_ctx in
  match force typ with
  | TyTuple ts -> TyTuple (List.map go ts)
  | TyArrow (a, b) -> TyArrow (go a, go b)
  | TyPoly p ->
      let[@warning "-8"] (Some p') = get_typevar l_ctx p in
      TyPoly p'
  | TyCustom (typ, args) ->
      let[@warning "-8"] (Some typ') = get_type ctx typ in
      let args = List.map go args in
      TyCustom (typ', args)
  | TyRef r -> TyRef (go r)
  | TyMeta m -> (
      match !m with
      (* safety: we need to preserve references, so this is _only_
         changing the type, which is unused
      *)
      | Unresolved -> TyMeta (Obj.magic m)
      | Resolved _ -> failwith "impossible: force")
  | TyBottom -> TyBottom
  | TyInt -> TyInt
  | TyString -> TyString
  | TyChar -> TyChar
  | TyFloat -> TyFloat
  | TyBool -> TyBool

let resolve_typdef ctx (typdef : 'a typdef) =
  let[@warning "-8"] (Some new_name) = get_type ctx typdef.name in
  let freshs = List.map resolved_using typdef.args in
  let l_ctx =
    new_local_ctx'
      (StrMap.of_list @@ List.combine typdef.args freshs)
      StrMap.empty
  in
  let content =
    match typdef.content with
    | Sum cases ->
        Sum
          (List.map
             (fun (name, tys) ->
               let[@warning "-8"] (Some name) =
                 get_constructor ctx name
               in
               let tys = List.map (resolve_type ctx l_ctx) tys in
               (name, tys))
             cases)
    | Record fields ->
        Record
          (List.map
             (fun (name, typ) ->
               let[@warning "-8"] (Some name) =
                 get_record_field ctx name
               in
               let ty = resolve_type ctx l_ctx typ in
               (name, ty))
             fields)
  in
  { data = typdef.data; name = new_name; args = freshs; content }

let rec resolve_case ctx l_ctx (case : 'a case) :
    local_ctx * resolved case =
  match case with
  | CaseWild -> (l_ctx, CaseWild)
  | CaseVar x ->
      let res = resolved_using x in
      let ctx' = add_local l_ctx x res in
      (ctx', CaseVar res)
  | CaseTuple ts ->
      let ctx', ts' =
        List.fold_left
          (fun (l_ctx, vars) curr ->
            let ctx', curr' = resolve_case ctx l_ctx curr in
            (ctx', curr' :: vars))
          (l_ctx, []) ts
      in
      (ctx', CaseTuple (List.rev ts'))
  | CaseCtor (ctor, cases) ->
      let ctx', cases' =
        List.fold_left
          (fun (l_ctx, vars) curr ->
            let ctx', curr' = resolve_case ctx l_ctx curr in
            (ctx', curr' :: vars))
          (l_ctx, []) cases
      in
      let[@warning "-8"] (Some ctor') = get_constructor ctx ctor in
      (ctx', CaseCtor (ctor', List.rev cases'))
  | CaseLit l -> (l_ctx, CaseLit l)

let rec resolve_expr ctx l_ctx (expr : (string, 'a) expr) :
    (resolved, 'a) expr =
  let go = resolve_expr ctx l_ctx in
  match expr with
  | Fail (d, s) -> Fail (d, s)
  | Var (d, nm) ->
      (* try locals first *)
      begin match get_local l_ctx nm with
      | Some res -> Var (d, res)
      | None -> (
          match get_global ctx nm with
          | Some res -> Var (d, res)
          | None -> failwith ("unknown variable: " ^ nm))
      end
  | MGlobal (_, _, _) -> failwith "monomorph info in name resolution"
  | Constructor (d, nm) ->
      let[@warning "-8"] (Some nm) = get_constructor ctx nm in
      Constructor (d, nm)
  | Int (d, s) -> Int (d, s)
  | String (d, s) -> String (d, s)
  | Char (d, s) -> Char (d, s)
  | Float (d, s) -> Float (d, s)
  | Bool (d, s) -> Bool (d, s)
  | LetIn (d, case, ty, head, body) ->
      let ty' = Option.map (resolve_type ctx l_ctx) ty in
      let head' = resolve_expr ctx l_ctx head in
      let l_ctx', case' = resolve_case ctx l_ctx case in
      let body' = resolve_expr ctx l_ctx' body in
      LetIn (d, case', ty', head', body')
  | Seq (d, a, b) -> Seq (d, go a, go b)
  | Funccall (d, f, x) -> Funccall (d, go f, go x)
  | BinOp (d, op, a, b) -> BinOp (d, op, go a, go b)
  | UnaryOp (d, op, a) ->
      let op' =
        match op with
        | Negate -> Negate
        | BNegate -> BNegate
        | Ref -> Ref
        | GetRecField nm ->
            let[@warning "-8"] (Some nm') = get_record_field ctx nm in
            GetRecField nm'
        | GetConstrField i -> GetConstrField i
        | Project i -> Project i
      in
      UnaryOp (d, op', go a)
  | Lambda (d, nm, ty, body) ->
      let nm' = resolved_using nm in
      let ty' = Option.map (resolve_type ctx l_ctx) ty in
      let l_ctx' = add_local l_ctx nm nm' in
      let body' = resolve_expr ctx l_ctx' body in
      Lambda (d, nm', ty', body')
  | Tuple (d, t) -> Tuple (d, List.map go t)
  | Annot (d, e, typ) -> Annot (d, go e, resolve_type ctx l_ctx typ)
  | Match (d, head, bodies) ->
      let head' = go head in
      let bodies' =
        List.map
          (fun (case, body) ->
            let l_ctx', case' = resolve_case ctx l_ctx case in
            let body' = resolve_expr ctx l_ctx' body in
            (case', body'))
          bodies
      in
      Match (d, head', bodies')
  | Modify (_, _, _) -> failwith "modify"
  | Record (dat, name, fields) ->
      let[@warning "-8"] (Some tname) = get_type ctx name in
      let fields =
        List.map
          (fun (nm, exp) ->
            let[@warning "-8"] (Some nm) = get_record_field ctx nm in
            let exp = go exp in
            (nm, exp))
          fields
      in
      Record (dat, tname, fields)

let resolve_definition ctx (def : ('a, 'b, yes) definition) =
  let[@warning "-8"] (Some name) = get_global ctx def.name in
  let new_typeargs = List.map resolved_using def.typeargs in
  let new_arg_vars =
    List.map (fun x -> resolved_using (fst x)) def.args
  in
  let l_ctx =
    new_local_ctx'
      (StrMap.of_list @@ List.combine def.typeargs new_typeargs)
      (StrMap.of_list
      @@ List.combine (List.map fst def.args) new_arg_vars)
  in
  let new_typs =
    List.map (fun x -> resolve_type ctx l_ctx (snd x)) def.args
  in
  let new_ret = resolve_type ctx l_ctx def.return in
  let new_body = Just (resolve_expr ctx l_ctx (get def.body)) in
  {
    data = def.data;
    name;
    typeargs = new_typeargs;
    args = List.combine new_arg_vars new_typs;
    return = new_ret;
    body = new_body;
  }

let rec resolve_top ctx (top : (string, 'b, unit) toplevel) :
    (resolved, 'b, void) toplevel list =
  let fixed =
    match top with
    | Module (nm, inners) ->
        (* find child ctx *)
        begin match
          List.find_opt (fun x -> x.module_name = nm) ctx.children
        with
        | None -> failwith "impossible: no module child"
        | Some child ->
            List.map (resolve_top child) inners |> List.flatten
        end
    | Typdef t -> Typdef (resolve_typdef ctx t) :: []
    | Definition d -> Definition (resolve_definition ctx d) :: []
    | Open _ -> (* we can ignore them *) []
  in
  fixed

let name_resolve (tops : (string, 'b, unit) toplevel list) :
    (resolved, 'b, void) toplevel list =
  let top_ctx = new_global_ctx "TOP" in
  let top_ctx = construct_global_ctx top_ctx tops in
  resolve_opens top_ctx tops;
  let fin = List.map (resolve_top top_ctx) tops in
  List.flatten fin
