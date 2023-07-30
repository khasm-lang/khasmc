open Ast
open Exp
open ListHelpers
(* Elimintates modules from a list of programs, reducing them to a
   flat file structure with fully resolved names *)

type module_ctx = {
  name : string;
  is_open : bool;
  idents : (string * bool) list;
  typs : string list;
  children : module_ctx list;
  parent : module_ctx ref option;
}

let empty_ctx () =
  {
    name = "&ktop&";
    is_open = false;
    idents = [];
    typs = [];
    children = [];
    parent = None;
  }

let ctx_with name parent =
  let child =
    {
      name;
      is_open = false;
      idents = [];
      typs = [];
      children = [];
      parent = Some (ref parent);
    }
  in
  let parent' = { parent with children = child :: parent.children } in
  (child, parent')

let rec close_all m =
  { m with is_open = false; children = List.map close_all m.children }

let add_ident ctx id = { ctx with idents = (id, false) :: ctx.idents }
let add_ident_extern ctx id = { ctx with idents = (id, true) :: ctx.idents }

let rec get_parent_list ctx =
  match ctx.parent with
  | None -> ctx.name
  | Some x -> get_parent_list !x ^ "." ^ ctx.name

let rec get_full_id ctx id =
  match List.assoc id ctx.idents with
  | true -> id
  | false -> get_parent_list ctx ^ "." ^ id

let elim_ts ctx ts = ts
let elim_expr ctx expr = expr

let rec elim_toplevel ctx t =
  match t with
  | TopAssign (t, a) ->
      let (id, ts), (_id, args, expr) = (t, a) in
      let ts' = elim_ts ctx ts in
      let expr' = elim_expr ctx expr in
      let id' = get_full_id ctx id in
      (add_ident ctx id, TopAssign ((id', ts'), (id', args, expr')))
  | TopAssignRec (t, a) ->
      let (id, ts), (_id, args, expr) = (t, a) in
      let id' = get_full_id ctx id in
      let ctx' = add_ident ctx id in
      let ts' = elim_ts ctx ts in
      let expr' = elim_expr ctx' expr in
      (ctx', TopAssignRec ((id', ts'), (id', args, expr')))
  | Extern (id, i, ts) -> (add_ident_extern ctx id, t)
  | IntExtern (id_internal, id_external, i, ts) ->
      (add_ident_extern ctx id_internal, t)
  | SimplModule (id, toplevels) -> todo "simpl modules"

and elim_toplevel_list ctx toplevels =
  match toplevels with
  | [] -> []
  | x :: xs ->
      let ctx', top = elim_toplevel ctx x in
      top :: elim_toplevel_list ctx' xs

let elim programs =
  List.map
    (fun t ->
      let (Program x) = t in
      Program (elim_toplevel_list (empty_ctx ()) x))
    programs
