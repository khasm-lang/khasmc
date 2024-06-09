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
  | TyMeta a, t | t, TyMeta a -> begin
      match get a with
      | Solved b -> unify' t b
      | Unsolved ->
          join_metas a (BUR.uref (Solved t));
          t
    end
  | Free a, Free b when a = b -> Free a
  | Custom a, Custom b when a = b -> Custom a
  | Tuple w, Tuple e -> Tuple (List.map2 unify' w e)
  | Arrow (a, b), Arrow (q, w) -> Arrow (unify' a q, unify' b w)
  | TApp (a, b), TApp (q, w) when a = q ->
      TApp (q, List.map2 unify' b w)
  | TForall _, t -> failwith "not sure yet"
  | _ -> raise (BadUnify (a, b))

let rec inst_frees (mapping : (string * meta BUR.t) list) (ty : ty) :
    (string * meta BUR.t) list * ty =
  match force ty with
  | Custom _ | TyInt | TyChar | TyBool | TyString | TyMeta _ ->
      (mapping, ty)
  | Free s -> begin
      match List.assoc_opt s mapping with
      | Some s -> (mapping, TyMeta s)
      | None ->
          let n = new_meta () in
          ((s, n) :: mapping, TyMeta n)
    end
  | Tuple w ->
      let map, t =
        List.fold_left
          (fun (acc, l) x ->
            let map, me = inst_frees acc x in
            (map, me :: l))
          (mapping, []) w
      in
      (map, Tuple t)
  | Arrow (a, b) ->
      let map, a = inst_frees mapping a in
      let map, b = inst_frees map b in
      (map, Arrow (a, b))
  | TApp (a, b) ->
      let map, t =
        List.fold_left
          (fun (acc, l) x ->
            let map, me = inst_frees acc x in
            (map, me :: l))
          (mapping, []) b
      in
      (map, TApp (a, t))
  | TForall (f, l) -> failwith "handle scoping issues"

let unify (a : ty) (b : ty) : (unit, 'a) result =
  let map, a = inst_frees [] a in
  let map, b = inst_frees map b in
  match unify' a b with
  | s -> ok ()
  | exception BadUnify (q, w) -> err @@ `Bad_Unify ((a, q), (b, w))
