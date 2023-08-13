open Ast
open Exp
open ListHelpers
(* Elimintates modules from a list of programs, reducing them to a
   flat file structure with fully resolved names *)

type module_ctx = {
  name : string;
  is_open : bool;
  (* bool is for mangling *)
  idents : (string * bool) list;
  typs : string list;
  children : module_ctx list;
  parent : module_ctx ref option;
} [@@deriving show {with_path = false}]

let get_open_children ctx =
  List.filter (fun x -> x.is_open) ctx.children

let open_single_module ctx id =
  match filter_extract (fun x -> x.name = id) ctx.children with
  | _, [] -> None
  | k, [x] -> Some({ctx with children = {x with is_open = true} :: k})
  | _, _ :: _ -> impossible "More then one valid module found to open"

let rec open_deep_module ctx id =
  match open_single_module ctx id with
  | Some(x) -> x
  | None ->
    match get_open_children ctx with
    | [] -> notfound ("Module not found: " ^ id)
    | x  ->
      match List.map (fun y -> open_deep_module y id) x with
      | [] -> notfound ("Module not found: " ^ id)
      | [x] -> x
      | _ -> impossible "More then one valid module found to open"


let empty_ctx =
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
 

let rec close_all m =
  { m with is_open = false; children = List.map close_all m.children }

let add_child ctx kid = {ctx with children = kid :: ctx.children}
let add_ident ctx id = { ctx with idents = (id, false) :: ctx.idents }
let add_ident_extern ctx id = { ctx with idents = (id, true) :: ctx.idents }

let rec get_parent_list ctx =
  match ctx.parent with
  | None -> ctx.name
  | Some x -> get_parent_list !x ^ "." ^ ctx.name

let rec get_full_id ctx id =
  match List.assoc_opt id ctx.idents with
  | Some(true) -> id
  | Some(false) | None -> get_parent_list ctx ^ "." ^ id

let elim_ts ctx ts = ts
let elim_expr ctx expr = expr

let rec elim_toplevel ctx t =
  match t with
  | TopAssign (t, a) ->
      let (id, ts), (_id, args, expr) = (t, a) in
      let ts' = elim_ts ctx ts in
      let expr' = elim_expr ctx expr in
      let id' = get_full_id ctx id in
      (add_ident ctx id', TopAssign ((id', ts'), (id', args, expr')) :: [])
  | TopAssignRec (t, a) ->
      let (id, ts), (_id, args, expr) = (t, a) in
      let id' = get_full_id ctx id in
      let ctx' = add_ident ctx id in
      let ts' = elim_ts ctx ts in
      let expr' = elim_expr ctx' expr in
      (ctx', TopAssignRec ((id', ts'), (id', args, expr')) :: [])
  | Extern (id, i, ts) ->
    (add_ident_extern ctx id, t :: [])
  | IntExtern (id_internal, id_external, i, ts) ->
      (add_ident_extern ctx id_external, t :: [])
  | Bind (id, ids, nm) ->
    if ids <> [] then todo "bind with module args" else
    let id' = get_full_id ctx id in
    (add_ident ctx id', Bind(id', [], nm) :: [])
  | Open (id) ->
    (open_deep_module ctx id, [])  
  | SimplModule (id, toplevels) ->
    let newctx = ctx_with id ctx in
    let ctx', top = elim_toplevel_list newctx toplevels in
    add_child ctx ctx', top 


and elim_toplevel_list ctx toplevels =
  match toplevels with
  | [] -> ctx, []
  | x :: xs ->
      let ctx', top = elim_toplevel ctx x in
      let ctx'', n = elim_toplevel_list ctx' xs in
      ctx'', top @ n 

let rec elim_helper ctx progs =
  match progs with
  | [] -> impossible "Empty programs"
  | [Program(x)] -> Program(snd @@ elim_toplevel_list ctx x) :: []
  | x :: xs ->
    let Program(x) = x in
    let ctx', prog = elim_toplevel_list ctx x in
    Program(prog) :: elim_helper ctx' xs

let elim programs =
  let tmp = elim_helper empty_ctx programs in
  tmp
