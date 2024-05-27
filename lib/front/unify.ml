open Ast
open Common.Info
open Common.Error
module BUR = BatUref

let new_meta () = BUR.uref Unsolved
let uref = BUR.uref
let set = BUR.uset
let get = BUR.uget

let join_metas (a : meta BUR.t) (b : meta BUR.t) : unit =
  match (BUR.uget a, BUR.uget b) with
  | Unsolved, Unsolved -> BUR.unite a b
  | Solved _, Unsolved -> BUR.unite ~sel:(fun a b -> a) a b
  | Unsolved, Solved _ -> BUR.unite ~sel:(fun a b -> b) a b
  | Solved _, Solved _ ->
      failwith "shouldn't be uniting two solved metas"

exception BadUnify of ty * ty

let rec unify' (a : ty) (b : ty) : ty =
  match (force a, force b) with
  | TyInt, TyInt
  | TyBool, TyBool
  | TyChar, TyChar
  | TyString, TyString ->
      a
  | TyMeta a, TyMeta b -> (
      match (get a, get b) with
      | Solved a, Solved b -> unify' a b
      | _ ->
          join_metas a b;
          TyMeta a)
  | Free a, Free b when a = b -> Free a
  | Custom a, Custom b when a = b -> Custom a
  | Tuple w, Tuple e -> Tuple (List.map2 unify' w e)
  | Arrow (a, b), Arrow (q, w) -> Arrow (unify' a q, unify' b w)
  | TApp (a, b), TApp (q, w) when a = q ->
      TApp (q, List.map2 unify' b w)
  | _ -> raise (BadUnify (a, b))

let unify (a : ty) (b : ty) : (ty, 'a) result =
  match unify' a b with
  | s -> ok s
  | exception BadUnify (q, w) -> err @@ `Bad_Unify ((a, q), (b, w))

exception BadKindUnify of kind * kind

let rec unify_kind' (k1 : kind) (k2 : kind) : bool =
  match (k1, k2) with
  | Star, Star -> true
  | KArrow (q, w), KArrow (a, b) -> unify_kind' q a && unify_kind' w b
  | _ -> false

let unify_kind k1 k2 : (kind, 'a) result =
  match unify_kind' k1 k2 with
  | _ -> ok k1
  | exception BadKindUnify (a, b) ->
      err @@ `Bad_Kind_Unify ((k1, a), (k2, b))
