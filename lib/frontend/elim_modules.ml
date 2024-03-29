open Ast
open Exp

open ListHelpers
(** Elimintates modules from a list of programs, reducing them to a
   flat file structure with fully resolved names *)

type local_ctx = { locals : string list }

let empty_lctx () = { locals = [] }
let add_local ctx local = { ctx with locals = local :: ctx.locals }
let add_locals ctx locals = List.fold_left add_local ctx locals
let from_args args = { locals = args }
let from_args_and_defaults args = { locals = khasm_default_types @ args }
let is_local ctx l = List.exists (fun x -> x = l) ctx.locals


(**
   Provides information about the current state of the modules in the
   program.
*)
type module_ctx = {
  name : string;
  (** Name of current module *)
  is_open : bool;
  (** Whether it's open *)
  idents : (string * bool) list;
  (** Identifiers avalible in the context (bool is whether they can be safely mangled) *)
  typs : string list;
  children : module_ctx list;
  parent : module_ctx ref option;
}
[@@deriving show { with_path = false }]

(** Opens a given module, searching by both child & parent *)
let rec open_if_valid id orig ctx =
  if ctx.name = id then
    { ctx with is_open = true }
  else
    open_deep_module_h ctx id orig

(** See open_if_valid *)
and open_deep_module_h ctx id orig =
  {
    ctx with
    is_open = ctx.name = id || ctx.is_open;
    children =
      List.map (open_if_valid id orig)
        (List.filter (fun x -> x.name <> orig) ctx.children);
    parent =
      (match ctx.parent with
      | None -> None
      | Some p -> Some (ref @@ open_deep_module_h !p id ctx.name));
  }

(** Helper for open_deep_module_h *)
let open_deep_module ctx id =
  let tmp = open_deep_module_h ctx id ctx.name in
  tmp

let empty_ctx () =
  {
    name = "khasm";
    is_open = true;
    idents = [];
    typs = [];
    children = [];
    parent = None;
  }

let ctx_with name parent =
  {
    name;
    is_open = false;
    idents = [];
    typs = [];
    children = [];
    parent = Some (ref parent);
  }
(** Closes all modules currently open *)
let rec close_all_h n m =
  {
    m with
    is_open = m.name = n;
    children = List.map (close_all_h m.name) m.children;
    parent =
      (match m.parent with
      | None -> None
      | Some p -> Some (ref @@ close_all_h m.name !p));
  }

(** Helper for close_all_h *)
let close_all m =
  let tmp = close_all_h m.name m in
  tmp

let add_child ctx kid =
  { ctx with children = { kid with is_open = false } :: ctx.children }

let add_ident ctx id = { ctx with idents = (id, false) :: ctx.idents }
let add_ident_extern ctx id = { ctx with idents = (id, true) :: ctx.idents }
let add_typ ctx typ = { ctx with typs = typ :: ctx.typs }


(** Helper for get_mang *)
let rec get_parent_list ctx =
  match ctx.parent with
  | None -> ctx.name
  | Some x -> get_parent_list !x ^ "." ^ ctx.name

(** Get a mangled name *)
let rec get_mang ctx id =
  if id = "main" then
    "main"
  else
    match List.assoc_opt id ctx.idents with
    | Some true -> id
    | Some false | None -> get_parent_list ctx ^ "." ^ id

(** Finds a function's full name from a context and a partial name *)
let rec get_full_from_open ctx id =
  match List.find_all (fun x -> fst x = id) ctx.idents with
  | [] -> (
      try
        match
          List.map
            (fun x -> get_full_from_open x id)
            (List.filter (fun x -> x.is_open) ctx.children)
        with
        | [] -> notfound ("Ident not found: " ^ id)
        | [ x ] -> get_mang ctx x
        | k -> ambigious ("Too many possible variables: " ^ String.concat "," k)
      with NotFound _ -> (
        match ctx.parent with
        | None -> notfound ("Ident not found: " ^ id)
        | Some p -> (
            let siblings = List.filter (fun x -> x.is_open) !p.children in
            match List.map (fun x -> get_full_from_open x id) siblings with
            | [] -> get_full_from_open !p id
            | [ x ] -> x
            | k ->
                ambigious
                  ("Too many possible vairiables: " ^ String.concat "," k))))
  | [ x ] -> get_mang ctx (fst x)
  | k ->
      ambigious
        ("Too many possible variables: " ^ String.concat "," (List.map fst k))

(** Finds a type's full name from a context and a partial name *)
let rec get_type_from_open ctx id =
  match List.find_all (fun x -> x = id) ctx.typs with
  | [] -> (
      try
        match
          List.map
            (fun x -> get_type_from_open x id)
            (List.filter (fun x -> x.is_open) ctx.children)
        with
        | [] -> notfound ("Ident not found: " ^ id)
        | [ x ] -> get_mang ctx x
        | k -> ambigious ("Too many possible variables: " ^ String.concat "," k)
      with NotFound _ -> (
        match ctx.parent with
        | None -> notfound ("Ident not found: " ^ id)
        | Some p -> (
            let siblings = List.filter (fun x -> x.is_open) !p.children in
            match List.map (fun x -> get_type_from_open x id) siblings with
            | [] -> get_type_from_open !p id
            | [ x ] -> get_mang ctx x
            | k ->
                ambigious
                  ("Too many possible vairiables: " ^ String.concat "," k))))
  | [ x ] -> get_mang ctx x
  | k -> ambigious ("Too many possible variables: " ^ String.concat "," k)

(** Opens a given module given a list of modules to open, like Open A.B.C *)
let rec deconstruct_modules ctx mods =
  match mods with
  | [] -> ctx
  | x :: xs -> (
      match List.find_all (fun y -> y.name = x) ctx.children with
      | [] -> (
          match ctx.parent with
          | None -> notfound ("Module not found: " ^ x)
          | Some p -> deconstruct_modules !p (x :: xs))
      | [ z ] -> deconstruct_modules z xs
      | k ->
          ambigious
            ("Too many possible modules: "
            ^ String.concat "," (List.map (fun x -> x.name) k)))

(** Wrapper for get_full_from_open *)
let rec get_full_id_mod ctx mods id =
  let ctx' = deconstruct_modules ctx mods in
  let tmp = get_full_from_open ctx' id in
  tmp

(** Wrapper for get_type_from_open *)
let rec get_full_typ_mod ctx mods id =
  let ctx' = deconstruct_modules ctx mods in
  let tmp = get_type_from_open ctx' id in
  tmp

(** Eliminate modules from a type signature *)
let rec elim_ts mctx lctx ts =
  match ts with
  | TSBase a ->
      if is_local lctx a then
        TSBase a
      else
        TSBase (get_full_typ_mod mctx [] a)
  | TSMeta a -> TSMeta a
  | TSApp (args, app) ->
      TSApp (List.map (elim_ts mctx lctx) args, get_full_typ_mod mctx [] app)
  | TSMap (f, x) -> TSMap (elim_ts mctx lctx f, elim_ts mctx lctx x)
  | TSForall (a, ts) ->
      let lctx' = add_local lctx a in
      TSForall (a, elim_ts mctx lctx' ts)
  | TSTuple l -> TSTuple (List.map (elim_ts mctx lctx) l)

(** Eliminate modules from a kexpr *)
let rec elim_base mctx lctx i k : kexpr =
  match k with
  | Ident (i', k) ->
      if is_local lctx k then
        Base (i, Ident (i', k))
      else
        Base (i, Ident (i', get_full_id_mod mctx [] k))
  | Int _ | Float _ | Str _ -> Base (i, k)
  | Tuple l -> Base (i, Tuple (List.map (elim_expr mctx lctx) l))
  | True | False -> Base (i, k)

(** Eliminate modules from a pattern *)
and elim_pat mctx lctx p =
  match p with
  | MPInt _ -> p
  | MPId _t -> p
  | MPApp (t, p) ->
      MPApp (get_full_id_mod mctx [] t, List.map (elim_pat mctx lctx) p)
  | MPTup t -> MPTup (List.map (elim_pat mctx lctx) t)
  | MPWild -> MPWild

(** Eliminate modules from many patterns *)
and elim_pats mctx lctx pats =
  List.map
    (fun (p, e) ->
      let p' = elim_pat mctx lctx p in
      let frees = get_pat_frees p' in
      let lctx' = add_locals lctx frees in
      (p', elim_expr mctx lctx' e))
    pats
    
(** Eliminate modules from an expr *)
and elim_expr mctx lctx expr =
  let default t = elim_expr mctx lctx t in
  match expr with
  | Base (i, k) -> elim_base mctx lctx i k
  | FCall (i, e1, e2) -> FCall (i, default e1, default e2)
  | LetIn (i, id, e1, e2) ->
      let ctx' = add_local lctx id in
      LetIn (i, id, default e1, elim_expr mctx ctx' e2)
  | LetRecIn (i, ts, id, e1, e2) ->
      let ctx' = add_local lctx id in
      LetRecIn (i, ts, id, elim_expr mctx ctx' e1, elim_expr mctx ctx' e2)
  | IfElse (i, c, e1, e2) -> IfElse (i, default c, default e1, default e2)
  | Join (i, e1, e2) -> Join (i, default e1, default e2)
  | Lam (i, id, expr) ->
      let ctx' = add_local lctx id in
      Lam (i, id, elim_expr mctx ctx' expr)
  | TypeLam (i, id, e) ->
      let ctx' = add_local lctx id in
      TypeLam (i, id, elim_expr mctx ctx' e)
  | TupAccess (i, e, i') -> TupAccess (i, default e, i')
  | AnnotLet (i, id, ts, e1, e2) ->
      let ctx' = add_local lctx id in
      AnnotLet (i, id, ts, default e1, elim_expr mctx ctx' e2)
  | AnnotLam (i, id, ts, e1) ->
      let ctx' = add_local lctx id in
      AnnotLam (i, id, ts, elim_expr mctx ctx' e1)
  | ModAccess (i, mods, id) ->
      let m = get_full_id_mod mctx mods id in
      Base (i, Ident ( i, m))
  | Match (i, p, pats) -> Match (i, p, elim_pats mctx lctx pats)

(** Eliminate modules from a toplevel *)
let rec elim_toplevel ctx t =
  match t with
  | TopAssign (info, id, ts, args, expr) ->
      let ts' = elim_ts ctx (from_args_and_defaults []) ts in
      let expr' = elim_expr ctx (from_args args) expr in
      let id' = get_mang ctx id in
      (add_ident ctx id, TopAssign (info, id', ts', args, expr') :: [])
  | TopAssignRec (info, id, ts, args, expr) ->
      let id' = get_mang ctx id in
      let ctx' = add_ident ctx id in
      let ts' = elim_ts ctx (from_args_and_defaults []) ts in
      let expr' = elim_expr ctx' (from_args args) expr in
      (ctx', TopAssignRec (info, id', ts', args, expr') :: [])
  | Extern (inf, id, i, ts) -> (add_ident_extern ctx id, t :: [])
  | IntExtern (inf, id_i, id_e, i, ts) ->
      let id' = get_mang ctx id_e in
      (add_ident ctx id_e, IntExtern (inf, id_i, id', i, ts) :: [])
  | Bind (inf, id, ids, nm) ->
      if ids <> [] then
        todo "bind with module args"
      else
        let id' = get_mang ctx id in
        let nm' = get_mang ctx nm in
        (add_ident ctx id, Bind (inf, id', [], nm') :: [])
  | Open (inf, id) -> (open_deep_module ctx id, [])
  | SimplModule (inf, id, toplevels) ->
      let newctx = ctx_with id ctx in
      let ctx', top = elim_toplevel_list newctx toplevels in
      let ctx' = close_all ctx' in
      (add_child ctx ctx', top)
  | Typedecl (inf, id, args, pats) ->
      let ctx' =
        List.fold_left (fun ctx pat -> add_ident ctx pat.head) ctx pats
      in
      let newctx = add_typ ctx' id in
      let rec conv_to_metas args ts =
        match ts with
        | TSBase a ->
            if List.mem a args then
              TSMeta a
            else
              ts
        | TSApp (a', b) -> TSApp (List.map (conv_to_metas args) a', b)
        | TSForall (a, b) -> TSForall (a, conv_to_metas args b)
        | TSMap (a, b) -> TSMap (conv_to_metas args a, conv_to_metas args b)
        | TSTuple t -> TSTuple (List.map (conv_to_metas args) t)
        | TSMeta _a -> ts
      in
      let mod_pats =
        List.map
          (fun x ->
            {
              head = get_full_id_mod newctx [] x.head;
              typ =
                (match x.typ with
                | Error () -> raise @@ Impossible "typ Error result"
                | Ok t -> (
                    match t with
                    | TSApp (x, y) ->
                        let tmp = get_full_typ_mod newctx [] y in
                        Ok (TSApp (x, tmp))
                    | x ->
                        print_endline "bad?";
                        Ok x));
              args =
                List.map (elim_ts newctx (from_args_and_defaults args)) x.args;
            })
          pats
      in
      (newctx, Typedecl (inf, get_full_typ_mod newctx [] id, args, mod_pats) :: [])
  | Typealias (inf, id, args, ts) ->
      let newctx = add_typ ctx id in
      let ts' = elim_ts ctx (from_args args) ts in
      (newctx, Typealias (inf, id, args, ts') :: [])

and elim_toplevel_list ctx toplevels =
  match toplevels with
  | [] -> (ctx, [])
  | x :: xs ->
      let ctx', top = elim_toplevel ctx x in
      let ctx'', n = elim_toplevel_list ctx' xs in
      (ctx'', top @ n)

let rec elim_helper ctx progs =
  match progs with
  | [] -> [ Program [] ]
  | x :: xs ->
      let (Program x) = x in
      let ctx', prog = elim_toplevel_list ctx x in
      Program prog :: elim_helper ctx' xs
        
(** Eliminate modules from a list of programs *)
let elim programs =
  let tmp = elim_helper (empty_ctx ()) programs in
  let rec all_but_last l =
    match l with
    | [] -> failwith "empty programs"
    | [ _ ] -> []
    | x :: xs -> x :: all_but_last xs
  in
  all_but_last tmp
