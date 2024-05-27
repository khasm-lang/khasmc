open Ast
open Common.Info
open Common.Error
open Common

type ctx = {
  (* short, full *)
  definitions : (path * path) list;
  included : path list;
  open' : path list;
  current : string;
}
[@@deriving show { with_path = false }]

let debug f p =
  print_string "\n------ DEBUG ------\n";
  print_string (f p);
  print_string "\n------  END  ------\n"

let add_file (ctx : ctx) (p : path) : path = InMod (ctx.current, p)

(* returns only the full matches as their longest path *)
let full_matches (paths : (path * path) list) (path : path) :
    path list =
  let rec go p1 p2 =
    match (p1, p2) with
    | End, End -> true
    | Base a, Base b when a = b -> true
    | InMod (a, q), InMod (b, w) ->
        if a = b then
          go q w
        else
          false
    | _ -> false
  in
  List.filter (fun pth -> go (fst pth) path) paths |> List.map snd

let rec similar_up_to path1 path2 =
  match (path1, path2) with
  | End, rest -> Some rest
  | InMod (a, q), InMod (b, w) when a = b -> similar_up_to q w
  | _ -> None

let rec find_naive_definition (ctx : ctx) (path : path) :
    (path, 'a) result =
  match path with
  | End -> failwith "Expected proper path, not empty"
  | Base n -> (
      match
        full_matches ctx.definitions (InMod (ctx.current, Base n))
      with
      | [] -> err (`No_Such_Variable path)
      | [ x ] -> ok x
      | _ -> err (`Overlapping_Variable path))
  | InMod (m, _tm) -> (
      match
        match full_matches ctx.definitions path with
        | [] -> err (`No_Such_Variable path)
        | [ x ] -> ok x
        | _ -> err (`Overlapping_Variable path)
      with
      | Ok s -> Ok s
      | Error e -> (
          match
            full_matches ctx.definitions (InMod (ctx.current, path))
          with
          | [] -> err (`No_Such_Variable path)
          | [ x ] -> ok x
          | _ -> err (`Overlapping_Variable path)))

let handle_opens (ctx : ctx) : ctx =
  let remove_one path =
    List.map (fun (s, long) ->
        match similar_up_to path long with
        | Some p -> (p, long)
        | None -> (s, long))
  in
  let new_defs =
    List.fold_left
      (fun acc nm -> remove_one nm acc)
      ctx.definitions ctx.open'
  in
  { ctx with definitions = new_defs; open' = [] }

let ensure_includes_correct ctx path =
  match path with
  | End -> failwith "Expected proper path, not empty"
  | Base _ -> ok path
  | _ -> (
      List.map
        (fun x -> similar_up_to x path)
        (InMod (ctx.current, End) :: ctx.included)
      |> List.filter (fun x ->
             match x with Some _ -> true | None -> false)
      |> function
      | [] ->
          err
            (`Bad_Include
              (" No such path wrt includes: " ^ show_path path))
      | _ -> ok path)

let find_definition (ctx : ctx) (path : path) : (path, 'a) result =
  let ctx = handle_opens ctx in
  let* pth = find_naive_definition ctx path in
  ensure_includes_correct ctx pth

let find_definition_string (ctx : ctx) (s : string) :
    (path, 'a) result =
  find_definition ctx (Base s)

let rec handle_ty (ctx : ctx) (ty : ty) : (ty, 'a) result =
  match ty with
  | TyString | TyInt | TyBool | TyChar | Free _ | TyMeta _ -> Ok ty
  | Custom p ->
      let+ path = find_definition ctx p in
      Custom path
  | Tuple t ->
      let+ tys = collect @@ List.map (handle_ty ctx) t in
      Tuple tys
  | Arrow (a, b) ->
      let+ a = handle_ty ctx a and+ b = handle_ty ctx b in
      Arrow (a, b)
  | TApp (p, tys) ->
      let+ p = find_definition ctx p
      and+ tys = collect @@ List.map (handle_ty ctx) tys in
      TApp (p, tys)
  | TForall (s, p) ->
      let+ t = handle_ty ctx p in
      TForall (s, t)

let rec get_bound (pat : pat) : string list =
  match pat with
  | Bind s -> [ s ]
  | PTuple s -> List.map get_bound s |> List.flatten
  | Constr (_, p) -> List.map get_bound p |> List.flatten

let rec handle_pat (ctx : ctx) (pat : pat) : (pat, 'a) result =
  match pat with
  | Bind _ -> ok pat
  | PTuple s ->
      let+ s = collect @@ List.map (handle_pat ctx) s in
      PTuple s
  | Constr (s, p) ->
      let+ s = find_definition ctx s
      and+ p = collect @@ List.map (handle_pat ctx) p in
      Constr (s, p)

let rec handle_tm (bound : string list) (ctx : ctx) (tm : tm) :
    (tm, 'a) result =
  match tm with
  | Var (id, tm) ->
      if List.mem tm bound then
        ok @@ Bound (id, Base tm)
      else
        let+ name = withid id @@ find_definition_string ctx tm in
        Bound (id, name)
  | Bound (id, nm) ->
      let+ name = withid id @@ find_definition ctx nm in
      Bound (id, name)
  | App (id, a, b) ->
      let+ a = handle_tm bound ctx a
      and+ b = collect @@ List.map (handle_tm bound ctx) b in
      App (id, a, b)
  | Let (id, pat, a, b) ->
      let bound' = get_bound pat in
      let+ a = handle_tm bound ctx a
      and+ b = handle_tm (bound' @ bound) ctx b
      and+ pat = handle_pat ctx pat in
      Let (id, pat, a, b)
  | Match (id, tm, mtch) ->
      let+ tm = handle_tm bound ctx tm
      and+ matches =
        collect
        @@ List.map
             (fun (a, b) ->
               let frees = get_bound a in
               let+ pat = handle_pat ctx a
               and+ body = handle_tm (frees @ bound) ctx b in
               (pat, body))
             mtch
      in
      Match (id, tm, matches)
  | Lam (id, pat, mbty, bd) ->
      let frees = get_bound pat in
      let+ pat = handle_pat ctx pat
      and+ ty = option_app (handle_ty ctx) mbty
      and+ bd = handle_tm (frees @ bound) ctx bd in
      Lam (id, pat, ty, bd)
  | ITE (id, a, b, c) ->
      let+ a = handle_tm bound ctx a
      and+ b = handle_tm bound ctx b
      and+ c = handle_tm bound ctx c in
      ITE (id, a, b, c)
  | Annot (id, tm, ty) ->
      let+ tm = handle_tm bound ctx tm and+ ty = handle_ty ctx ty in
      Annot (id, tm, ty)
  | Record (id, nm, fields) ->
      let+ nm = find_definition ctx nm
      and+ fields =
        collect
        @@ List.map
             (fun (nm, tm) ->
               let+ tm = handle_tm bound ctx tm in
               (nm, tm))
             fields
      in
      Record (id, nm, fields)
  | Project (id, path, field) ->
      let+ path = find_definition ctx path in
      Project (id, path, field)
  | Poison (id, exn) -> ok @@ Poison (id, exn)

let handle_constraints (ctx : ctx) (tm : constraint') :
    (constraint', 'a) result =
  let nm, tys = tm in
  let+ nm = find_definition ctx nm
  and+ tys = collect @@ List.map (handle_ty ctx) tys in
  (nm, tys)

let handle_definition (ctx : ctx)
    { name; free_vars; constraints; args; ret; body } :
    (definition, 'a) result =
  let* cons =
    collect @@ List.map (handle_constraints ctx) constraints
  in
  let arg1, arg2 = List.split args in
  let+ tys = collect @@ List.map (handle_ty ctx) arg2
  and+ ret = handle_ty ctx ret
  and+ body = handle_tm arg1 ctx body in
  let args = List.combine arg1 tys in
  {
    name = add_file ctx name;
    free_vars;
    constraints = cons;
    args;
    ret;
    body;
  }

let rec handle_tyexpr (ctx : ctx) (tyexpr : tyexpr) :
    (tyexpr, 'a) result =
  match tyexpr with
  | TVariant t ->
      let+ t =
        collect
        @@ List.map
             (fun (nm, tys) ->
               let+ tys = collect @@ List.map (handle_ty ctx) tys in
               (nm, tys))
             t
      in
      TVariant t
  | TRecord t ->
      let+ t =
        collect
        @@ List.map
             (fun (nm, ty) ->
               let+ ty = handle_ty ctx ty in
               (nm, ty))
             t
      in
      TRecord t
  | TAlias t ->
      let+ t = handle_ty ctx t in
      TAlias t

let rec handle_trait (ctx : ctx) (trait : trait) : (trait, 'a) result
    =
  let+ constraints =
    collect @@ List.map (handle_constraints ctx) trait.constraints
  and+ functions =
    collect @@ List.map (handle_definition ctx) trait.functions
  in
  {
    trait with
    name = add_file ctx trait.name;
    constraints;
    functions;
  }

let rec handle_impl (ctx : ctx) (impl : impl) : (impl, 'a) result =
  let+ args = collect @@ List.map (handle_ty ctx) impl.args
  and+ assoc_types =
    collect
    @@ List.map
         (fun (a, b) ->
           let+ b = handle_ty ctx b in
           (a, b))
         impl.assoc_types
  and+ impls =
    collect @@ List.map (handle_definition ctx) impl.impls
  in
  { name = add_file ctx impl.name; args; assoc_types; impls }

let rec base_name b =
  match b with
  | Base n -> n
  | InMod (_, n) -> base_name n
  | End -> failwith "shouldn't be end"

let get_constr_paths file nm def =
  match def with
  | TVariant xs ->
      List.map fst xs
      |> List.map (fun x ->
             InMod (file.name, InMod (base_name nm, Base x)))
  | TRecord _ -> InMod (file.name, Base (base_name nm)) :: []
  | TAlias t -> []

let handle_file (ctx : ctx) (file : file) : (file * ctx, 'a) result =
  let collect_names =
    List.map
      (function
        | Definition (_, dfn) -> InMod (file.name, dfn.name) :: []
        | Type (_, def) ->
            InMod (file.name, def.name)
            :: get_constr_paths file def.name def.expr
        | Trait (_, dfn) -> InMod (file.name, dfn.name) :: []
        | Impl (_, impl) -> InMod (file.name, impl.name) :: [])
      file.toplevel
    |> List.flatten
    |> List.map (fun x -> (x, x))
  in
  let ctx =
    { ctx with definitions = collect_names @ ctx.definitions }
  in
  let+ defs =
    collect
    @@ List.map
         (function
           | Definition (id, dfn) ->
               let+ def = handle_definition ctx dfn in
               Definition (id, def)
           | Type (id, def) ->
               let+ t = handle_tyexpr ctx def.expr in
               Type
                 ( id,
                   { def with name = add_file ctx def.name; expr = t }
                 )
           | Trait (id, trait) ->
               let+ t = handle_trait ctx trait in
               Trait (id, t)
           | Impl (id, impl) ->
               let+ t = handle_impl ctx impl in
               Impl (id, t))
         file.toplevel
  in
  ({ file with toplevel = defs }, ctx)

let handle_files files =
  (* currently handles them in given order
     TODO: implement some sort of proper solver or something
  *)
  let ctx =
    { definitions = []; included = []; open' = []; current = "" }
  in
  let rec go files ctx =
    match files with
    | [] -> ok []
    | x :: xs -> (
        match handle_file { ctx with current = x.name } x with
        | Ok (r, ctx) ->
            let+ rest = go xs ctx in
            r :: rest
        | Error e -> Error e)
  in
  match go files ctx with
  | Ok s -> s
  | Error e ->
      let rec errfmt (e : 'a) : (id * string) list =
        match e with
        | `Bad_Include s -> (noid, "Bad include: " ^ s) :: []
        | `Id (i, e) ->
            let t = List.flatten @@ List.map errfmt e in
            List.map (fun x -> (i, snd x)) t
        | `No_Such_Variable s ->
            (noid, "No such variable: " ^ show_path s) :: []
        | `Overlapping_Variable s ->
            (noid, "Overlapping variable: " ^ show_path s) :: []
      in
      let tmp = List.flatten @@ List.map errfmt e in
      let formatted = List.map (fun (a, b) -> format_error a b) tmp in
      let total = String.concat "\n\n" formatted in
      compiler_error Frontend' total
