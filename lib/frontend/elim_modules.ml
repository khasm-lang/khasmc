open Ast
open Exp

let rec list_without f l s =
  match l with
  | [] -> []
  | x :: xs -> if f x s then list_without f xs s else x :: list_without f xs s

let rec starts_with l s =
  match (l, s) with
  | [], [] -> true
  | _, [] -> true
  | x :: xs, y :: ys -> x = y && starts_with xs ys
  | _, _ -> false

let rec remove_using s l =
  if not (starts_with l s) then l
  else
    match (l, s) with
    | [], [] -> []
    | x, [] -> x
    | _ :: xs, _ :: ys -> remove_using xs ys
    | _, _ -> []

let rec ends_with l s =
  if List.length l = List.length s then l = s
  else if List.length l < List.length s then false
  else
    match l with
    | [] -> raise @@ Impossible "nolen s starts_with"
    | _ :: xs -> starts_with xs s

let rec join_on s l =
  match l with [] -> "" | [ x ] -> x | x :: xs -> x ^ s ^ join_on s xs

let rec last l =
  match l with
  | [] -> raise @@ Impossible "empty list last"
  | [ x ] -> x
  | _ :: xs -> last xs

type elim_ctx = {
  name : string;
  mods : elim_ctx list;
  prefix : string list;
  vars : (string list * string list) list;
  predefined : (string list * string list) list;
}
[@@deriving show { with_path = false }]

let empty_elim_ctx () =
  { mods = []; name = ""; prefix = []; vars = []; predefined = [] }

let ctx_with parent nm =
  {
    mods = parent.mods;
    name = nm;
    prefix = parent.prefix @ [ nm ];
    vars = [];
    predefined = parent.vars @ parent.predefined;
  }

let add_unqual_var ctx nm =
  { ctx with vars = ([ nm ], ctx.prefix @ [ nm ]) :: ctx.vars }

let add_qual_var ctx nm = { ctx with vars = (nm, ctx.prefix @ nm) :: ctx.vars }
let add_local_var ctx nm = { ctx with vars = ([ nm ], [ nm ]) :: ctx.vars }
let add_module ctx1 ctx2 = { ctx1 with mods = ctx2 :: ctx1.mods }

let open_module_qual ctx name =
  let tmp = List.find_opt (fun x -> x.name = name) ctx.mods in
  let tmp = match tmp with Some s -> s | None -> raise @@ NotFound name in
  let without = list_without (fun x y -> x.name = y) ctx.mods name in
  let with_prefix =
    List.map (fun x -> (last tmp.prefix :: fst x, snd x)) tmp.vars
  in
  let ctx' =
    { ctx with predefined = with_prefix @ ctx.predefined; mods = without }
  in
  ctx'

let open_module ctx name =
  {
    ctx with
    predefined =
      List.map
        (fun x ->
          let a, b = x in
          match a with
          | [] -> raise @@ Impossible "empty variable"
          | [ x ] -> ([ x ], b)
          | x :: xs -> if x = name then (xs, b) else (x :: xs, b))
        ctx.predefined;
  }

let rec find_proper_name_vars vars ctx unqual =
  match vars with
  | [] -> raise @@ NotFound (join_on "." unqual)
  | x :: xs ->
      if fst x = unqual then snd x else find_proper_name_vars xs ctx unqual

let fix_name ctx id =
  if id = "main" then id else join_on "." (ctx.prefix @ [ id ])

let rec find_proper_name ctx unqual =
  try find_proper_name_vars ctx.vars ctx [ unqual ]
  with NotFound _ -> find_proper_name_vars ctx.predefined ctx [ unqual ]

let rec find_proper_name_mod ctx unqual =
  try find_proper_name_vars ctx.vars ctx unqual
  with NotFound _ -> find_proper_name_vars ctx.predefined ctx unqual

let rec elim_expr ctx expr =
  match expr with
  | Base (i1, Ident (i2, id)) ->
      let id' = join_on "." @@ find_proper_name ctx id in
      Base (i1, Ident (i2, id'))
  | Base (i1, Tuple l) -> Base (i1, Tuple (List.map (elim_expr ctx) l))
  | Base (_, _) -> expr
  | FCall (i, e1, e2) -> FCall (i, elim_expr ctx e1, elim_expr ctx e2)
  | LetIn (i, id, e1, e2) ->
      let ctx' = add_local_var ctx id in
      LetIn (i, id, elim_expr ctx e1, elim_expr ctx' e2)
  | IfElse (i, c, e1, e2) ->
      IfElse (i, elim_expr ctx c, elim_expr ctx e1, elim_expr ctx e2)
  | Join (i, e1, e2) -> Join (i, elim_expr ctx e1, elim_expr ctx e2)
  | Inst (_, _, _) -> raise @@ Impossible "inst"
  | Lam (i, id, e) ->
      let ctx' = add_local_var ctx id in
      Lam (i, id, elim_expr ctx' e)
  | TypeLam (i, id, e) -> TypeLam (i, id, elim_expr ctx e)
  | TupAccess (i, e, l) -> TupAccess (i, elim_expr ctx e, l)
  | AnnotLet (i, id, ts, e1, e2) ->
      let ctx' = add_local_var ctx id in
      AnnotLet (i, id, ts, elim_expr ctx e1, elim_expr ctx' e2)
  | AnnotLam (i, id, ts, e) ->
      let ctx' = add_local_var ctx id in
      AnnotLam (i, id, ts, elim_expr ctx' e)
  | ModAccess (i, l, e) ->
      let whole = l @ [ e ] in
      let id' = join_on "." @@ find_proper_name_mod ctx whole in
      Base (i, Ident (dummy_info (), id'))

let rec elim_toplevel ctx t =
  match t with
  | TopAssign ((id, ts), (_id, args, expr)) ->
      let ctx' = List.fold_left (fun x y -> add_local_var x y) ctx args in
      let trans = elim_expr ctx' expr in
      let ctx'' = add_unqual_var ctx id in
      let proper = fix_name ctx id in
      (ctx'', TopAssign ((proper, ts), (proper, args, trans)) :: [])
  | TopAssignRec ((id, ts), (_id, args, expr)) ->
      let ctx' = add_unqual_var ctx id in
      let ctx'' = List.fold_left (fun x y -> add_local_var x y) ctx' args in
      let trans = elim_expr ctx'' expr in
      let proper = fix_name ctx id in
      (ctx', TopAssignRec ((proper, ts), (proper, args, trans)) :: [])
  | Extern (i, ts) ->
      let ctx' = add_unqual_var ctx i in
      let proper = fix_name ctx i in
      (ctx', Extern (proper, ts) :: [])
  | IntExtern (i, a, ts) ->
      let ctx' = add_unqual_var ctx a in
      let proper = fix_name ctx a in
      (ctx', IntExtern (i, proper, ts) :: [])
  | SimplModule (i, tl) ->
      let ctx' = ctx_with ctx i in
      let ctx'', tl' = elim_toplevel_list ctx' tl in
      let ctx''' = add_module ctx ctx'' in
      let ctx'''' = open_module_qual ctx''' i in
      (ctx'''', tl')
  | Bind (op, mods, id) ->
      let get = join_on "." @@ find_proper_name_mod ctx (mods @ [ id ]) in
      let ctx' = add_unqual_var ctx op in
      let name = fix_name ctx op in
      (ctx', Bind (name, [], get) :: [])

and elim_toplevel_list ctx tl =
  match tl with
  | [] -> (ctx, [])
  | x :: xs ->
      let ctx', e = elim_toplevel ctx x in
      let ctx'', tl = elim_toplevel_list ctx' xs in
      (ctx'', e @ tl)

let rec elim_program ctx p =
  let (Program tl) = p in
  elim_toplevel_list ctx tl

let rec elim_proglist ctx proglist =
  match proglist with
  | [] -> []
  | x :: xs ->
      let ctx', p = elim_program ctx x in
      Program p :: elim_proglist ctx' xs

let elim proglist = elim_proglist (empty_elim_ctx ()) proglist
