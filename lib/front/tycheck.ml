open Ast
open Common.Info
open Common.Error
module BUR = BatUref
open Unify

type info += Type
type data += Type' of ty
type info += IsTrait
type data += IsTrait'

let is_trait id = set_property id IsTrait IsTrait'
let set_ty id ty = set_property id Type (Type' ty)
let get_ty id = get_property id Type

let rec is_rank_one' (t : ty) : bool =
  match force t with
  | Tuple t -> List.for_all is_rank_one' t
  | Arrow (a, b) -> is_rank_one' a && is_rank_one' b
  | TApp (_, l) -> List.for_all is_rank_one' l
  | TForall _ -> false
  | _ -> true

let rec is_rank_two' (t : ty) : bool =
  match force t with
  | Tuple t -> List.for_all is_rank_two' t
  | Arrow (a, b) -> is_rank_two' a && is_rank_two' b
  | TApp (_, l) -> List.for_all is_rank_two' l
  | TForall (_, p) -> is_rank_one' p
  | _ -> true

let is_rank_one (x : ty) : (ty, 'a) result =
  match is_rank_one' x with
  | true -> Ok x
  | false -> err @@ `Is_Not_Rank_One x

let is_rank_two (x : ty) : (ty, 'a) result =
  match is_rank_two' x with
  | true -> Ok x
  | false -> err @@ `Is_Not_Rank_Two x

let match' x cont =
  let+ x = x in
  cont x

type ctx = {
  bound : (string * id * ty') list;
  types : typ list;
  frees : (string * kind) list;
  locals : (string * id * ty') list;
}
[@@deriving show { with_path = false }]

let empty () = { bound = []; types = []; frees = []; locals = [] }

let add_def ctx nm id ty =
  { ctx with bound = (nm, id, ty) :: ctx.bound }

let add_typ ctx t = { ctx with types = t :: ctx.types }

let add_local ctx nm id ty =
  { ctx with locals = (nm, id, ty) :: ctx.locals }

let add_frees ctx frees = { ctx with frees = frees @ ctx.frees }

let add_args ctx id args =
  {
    ctx with
    locals =
      List.map (fun (a, b) -> (a, id, ([], b))) args @ ctx.locals;
  }

(* TODO: also search by free vars *)
let find_type_by_nm (ctx : ctx) (p : path) : (typ, 'a) result =
  match List.filter (fun (x : typ) -> x.name = p) ctx.types with
  | [ x ] -> ok x
  | _ -> err @@ `Impossible "find_type_by_nm"

let process_trait (ctx : ctx) (id : id)
    ({ name; args; assoc_types; constraints; functions } : trait) :
    ctx =
  (* we treat the trait functions as "magic" for the moment,
     giving them the most general type we can
  *)
  let rec go (ctx : ctx) : definition list -> ctx = function
    | [] -> ctx
    | def :: xs ->
        let ctx' =
          add_def ctx (to_str def.name) id
            (def.free_vars, mk_ty (List.map snd def.args) def.ret)
        in
        go ctx' xs
  in
  go ctx functions

let collect_statement (ctx : ctx) (state : statement) : ctx =
  match state with
  | Definition (id, def) ->
      let ty = mk_ty (List.map snd def.args) def.ret in
      add_def ctx (to_str def.name) id (def.free_vars, ty)
  | Type (id, typ) -> add_typ ctx typ
  | Trait (id, trait) -> process_trait ctx id trait
  | Impl (_, _) -> ctx (* we ignore impls for the moment *)

let rec typecheck_tm (ctx : ctx) (tm : tm) : (tm, 'a) result =
  failwith "typecheck tm"

let rec check_ty (ctx : ctx) (ty : ty) (k : kind) : (unit, 'a) result
    =
  match (force ty, k) with
  | TyInt, Star | TyBool, Star | TyChar, Star | TyString, Star ->
      ok ()
  | Free a, k ->
      let k' = List.assoc a ctx.frees in
      let+ _ = unify_kind k k' in
      ()
  | Custom p, k ->
      let* def = find_type_by_nm ctx p in
      let+ _ = unify_kind def.kind k in
      ()
  | Tuple t, Star ->
      let+ _ = collect @@ List.map (fun x -> check_ty ctx x Star) t in
      ()
  | Arrow (a, b), Star ->
      let+ _ = check_ty ctx a Star and+ _ = check_ty ctx b Star in
      ()
  | TApp (p, l), k ->
      let rec go typ list : (kind, 'a) result =
        match (typ, list) with
        | KArrow (a, b), x :: xs ->
            let* _ = unify_kind a x in
            go b xs
        | Star, x :: xs -> err @@ `Kind_App_Doesn't_Match (p, l)
        | t, [] -> ok t
      in
      let* def = find_type_by_nm ctx p in
      let* args = collect @@ List.map (infer_ty ctx) l in
      let* ret = go def.kind args in
      let+ _ = unify_kind ret k in
      ()
  | TForall (l, inner), Star -> failwith "add inners to ctx, continue"
  | _ -> err @@ `Check_Ty_Fail (ty, k)

and infer_ty (ctx : ctx) (ty : ty) : (kind, 'a) result =
  failwith "infer types"

let typecheck_ty (ctx : ctx) (ty : ty) : (unit, 'a) result =
  let* ty = is_rank_two ty in
  check_ty ctx ty Star

let typecheck_statement (ctx : ctx) (s : statement) :
    (statement, 'a) result =
  match s with
  | Definition (id, def) ->
      let ctx' = add_frees ctx def.free_vars in
      let ctx' = add_args ctx' id def.args in
      let+ _ = typecheck_tm ctx' def.body
      and+ _ =
        collect
        @@ List.map (fun (a, b) -> typecheck_ty ctx' b) def.args
      and+ _ = typecheck_ty ctx' def.ret in
      Definition (id, def)
  | Impl (id, impl) -> failwith "todo: typecheck impl"
  | _ -> ok s

let typecheck (files : statement list) : statement list =
  let ctx = List.fold_left collect_statement (empty ()) files in
  match collect @@ List.map (typecheck_statement ctx) files with
  | Ok s -> s
  | Error e -> failwith "handle errors"
