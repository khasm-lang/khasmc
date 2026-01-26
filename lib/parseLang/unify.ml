open Share.Uuid
open Share.Result
open Parsing.Ast

(*
  woooooooooooo unification
  we do a really simple forwarding-based meta system,
  so our unification implementation is equally simple
  yay
 *)

(* pretty straightforward unification *)
let rec unify_h (t1 : 'a typ) (t2 : 'a typ) : ('a typ, string) result
    =
  match (force t1, force t2) with
  | TyInt, TyInt -> ok TyInt
  | TyString, TyString -> ok TyString
  | TyChar, TyChar -> ok TyChar
  | TyFloat, TyFloat -> ok TyFloat
  | TyBool, TyBool -> ok TyBool
  | TyTuple t1, TyTuple t2 ->
      let* t =
        List.map2 unify_h t1 t2
        |> collect
        |> Result.map_error (String.concat " ")
      in
      ok @@ TyTuple t
  | TyArrow (a, b), TyArrow (q, w) ->
      let* a' = unify_h a q in
      let* b' = unify_h b w in
      ok @@ TyArrow (a', b')
  | TyPoly a, TyPoly b ->
      (* if polys are equal, they must be fine
        we already know that non-local polys have been instantiated
        by this point, so we're all good in that department
      *)
      if a = b then
        ok @@ TyPoly a
      else
        err "can't unify_h uneq polys"
  | TyCustom (x, xs), TyCustom (y, ys) ->
      if x <> y then
        err "can't unify_h not equals"
      else
        List.map2 unify_h xs ys
        |> collect
        |> Result.map_error (String.concat " ")
        |> fun xs ->
        let* xs = xs in
        ok @@ TyCustom (x, xs)
  | TyRef a, TyRef b -> unify_h a b
  | TyMeta a, TyMeta b ->
      (* we did a force, so both should be unsolved *)
      begin match !a with
      | Unresolved ->
          (* set one to the other *)
          a := Resolved t2;
          ok t2
      | Resolved _ -> failwith "impossible"
      end
  | TyMeta a, t | t, TyMeta a -> begin
      match !a with
      | Resolved _ -> failwith "impossible"
      | Unresolved ->
          a := Resolved t;
          ok t
    end
  | a, b ->
      let f = show_typ pp_resolved in
      err @@ "these don't unify bozo: " ^ f a ^ "\n & \n" ^ f b

let unify polys a b =
  (* ensure that we get rid of any non-local polys so that we
     don't end up "losing" polymorphism
   *)
  let a = to_metas polys a in
  let b = to_metas polys b in
  (* should we be regeneralizing here? quite possibly
     TODO: check
   *)
  unify_h a b

(* TODO: don't use this anywhere *)
let unify' polys a b = ignore @@ unify polys a b
