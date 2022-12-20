open Exp
open Ast
open Uniq_typevars


type ctx = {
    typvars : kident list;
    binds : (kident * typesig) list;
  }

let is_typvar ctx x =
  List.mem ctx.typvars x

let lookup ctx x =
  match List.find_opt (fun y -> fst y = x) ctx.binds with
  | None -> raise (NotFound(x ^ " not found in ctx"))
  | Some(x) -> snd x

let rec subs typ nm newt =
  match typ with
  | TSBase(x) ->
     if x == nm then
       newt
     else
       typ
  | TSMeta(x) -> typ
  | TSApp(x, y) -> TSApp(subs x nm newt, y)
  | TSMap(l, r) -> TSMap(subs l nm newt, subs r nm newt)
  | TSForall(nm', ts) ->
     if nm' == nm then
       typ
     else
       TSForall(nm', subs ts nm newt)
  | TSTuple(l) -> TSTuple(List.map (fun x -> subs x nm newt) l)

type unify_ctx = {
    metas : (string * typesig) list;
  }
[@@deriving show {with_path = false}]

let empty_unify_ctx () = {metas = []}

let get_meta_opt ctx m =
  List.find_opt (fun x -> fst x = m) ctx.metas 

let uniq_cons x xs = if List.mem x xs then xs else x :: xs

let mk_uniq_list xs = List.fold_right uniq_cons xs []

let combine_uniq x y =
  mk_uniq_list (x @ y)

let combine ctx1 ctx2 =
  let rec helper a b =
    match a with
    | [] -> b
    | x :: xs ->
       match List.find_opt (fun y -> fst y = fst x) b with
       | None -> x :: helper xs b
       | Some(k) ->
          if snd k = snd x then
            helper xs b
          else
            raise (UnifyErr ("metavariable " ^ fst x ^ " has both"
                             ^ " type "
                             ^ pshow_typesig (snd k)
                             ^ " and "
                             ^ pshow_typesig (snd x)
                             ^ " within unification"))
  in
  {metas = helper ctx1.metas ctx2.metas}

let rec unify_list ctx l1 l2 =
  match (l1, l2) with
  | ([], []) -> (ctx, [])
  | (x :: xs, y :: ys) ->
     let tmp = unify ctx x y in
     let rest = unify_list (fst tmp) xs ys in
     ((fst rest), (snd tmp) :: (snd rest))
  | (_, _) -> raise (UnifyErr ("unequal tuple len"))


(* TODO
Credit to AradArbel10 on rpl

if you're doing this in ocaml (or any language with ref cells) there's actually a really neat trick to optimize (and simplify 😄) metavars.
```
type meta_state =
| Solved of typ
| Unsolved of name (* used for pretty printing *)
and typ =
| Arrow of typ * typ
| ...
| Meta of meta_state ref
```
so now the metavar is mutable, and when you solve it it gets "propagated up" automatically. actually implementing it introduces a couple of other subtleties but I think it's worth it.
 *)

and unify ctx l r =
  match (l, r) with
  | (TSBase(x), TSBase(y)) ->
     if x = y then
       (ctx, TSBase(x))
     else
       raise (UnifyErr ("can't unify " ^ x ^ " and " ^ y))
  | (TSMap(a, b), TSMap(x, y)) ->
     let lt = unify ctx a x in
     let rt = unify ctx b y in
     (combine (fst lt) (fst rt), TSMap((snd lt), (snd rt)))
  | (TSApp(a, b), TSApp(x, y)) ->
     if b <> y then
       raise (UnifyErr ("can't unify" ^ b ^ " and " ^ y))
     else
       let t = unify ctx a x in
       (fst t, TSApp(snd t, b))
  | (TSForall(a, b), TSForall(x, y)) ->
     let sub = subs y x (TSBase(a)) in
     let unf = unify ctx sub b in 
     (fst unf, TSForall(x, snd unf))
  | (TSTuple(a), TSTuple(x)) ->
     let tmp = unify_list ctx a x in
     (fst tmp, TSTuple(snd tmp))
  | (TSMeta(m), _) ->
     begin
       match get_meta_opt ctx m with
       | Some(x) ->
          let typ = snd x in
          unify ctx typ r
       | None ->
          (
            combine {metas = [(m, r)]} ctx,
            r
          )
     end
  | (_, _) -> raise (NotImpl ("unify - other cases?"))

let rec infer_base ctx tm =
  match tm with
  | Ident(i) -> lookup ctx i
  | Int(_) -> TSBase("int")
  | Float(_) -> TSBase("float")
  | Str(_) -> TSBase("string")
  | Tuple(l) -> TSTuple(List.map (infer ctx) l)
  | True | False -> TSBase("Bool")

and infer ctx tm =
  match tm with
  | Base(x) -> infer_base ctx x
  | Paren(x) -> infer ctx x

