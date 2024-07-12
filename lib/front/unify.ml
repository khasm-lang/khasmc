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

let rec unify' cont (a : ty) (b : ty) : ty =
  match (force a, force b) with
  | TyInt, TyInt
  | TyBool, TyBool
  | TyChar, TyChar
  | TyString, TyString ->
      a
  | TyMeta a, TyMeta b -> (
      match (get a, get b) with
      | Solved a, Solved b -> unify' cont a b
      | _ ->
          join_metas a b;
          TyMeta a)
  | TyMeta a, t | t, TyMeta a -> begin
      match get a with
      | Solved b -> unify' cont t b
      | Unsolved ->
          join_metas a (BUR.uref (Solved t));
          t
    end
  | Free a, Free b when a = b -> Free a
  | Custom a, Custom b when a = b -> Custom a
  | Tuple w, Tuple e -> Tuple (List.map2 (unify' cont) w e)
  | Arrow (a, b), Arrow (q, w) ->
      Arrow ((unify' cont) a q, (unify' cont) b w)
  | TApp (a, b), TApp (q, w) when a = q ->
      TApp (q, List.map2 (unify' cont) b w)
  | TForall _, t -> Common.Log.fatal "not sure yet"
  | a, b -> cont a b

let rec inst_frees' (exclude : string list)
    (mapping : (string * meta BUR.t) list) (ty : ty) :
    (string * meta BUR.t) list * ty =
  match force ty with
  | Custom _ | TyInt | TyChar | TyBool | TyString | TyMeta _ ->
      (mapping, ty)
  | Free s -> begin
      if List.mem s exclude then
        (mapping, Free s)
      else
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
            let map, me = inst_frees' exclude acc x in
            (map, me :: l))
          (mapping, []) w
      in
      (map, Tuple t)
  | Arrow (a, b) ->
      let map, a = inst_frees' exclude mapping a in
      let map, b = inst_frees' exclude map b in
      (map, Arrow (a, b))
  | TApp (a, b) ->
      let map, t =
        List.fold_left
          (fun (acc, l) x ->
            let map, me = inst_frees' exclude acc x in
            (map, me :: l))
          (mapping, []) b
      in
      (map, TApp (a, t))
  | TForall (f, l) -> failwith "handle scoping issues"

let inst_frees exclude x = snd @@ inst_frees' exclude [] x

let unify exe (a : ty) (b : ty) : (unit, 'a) result =
  let map, a = inst_frees' exe [] a in
  let map, b = inst_frees' exe map b in
  let cont a b = raise (BadUnify (a, b)) in
  match unify' cont a b with
  | s -> ok ()
  | exception BadUnify (q, w) -> err @@ `Bad_Unify ((a, q), (b, w))

let unify_no_inst (a : ty) (b : ty) : (unit, 'a) result =
  let cont a b = raise (BadUnify (a, b)) in
  match unify' cont a b with
  | s -> ok ()
  | exception BadUnify (q, w) -> err @@ `Bad_Unify ((a, q), (b, w))

let unify_b exe a b =
  match unify exe a b with Ok _ -> true | Error _ -> false
