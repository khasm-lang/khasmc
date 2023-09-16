open Exp

open Khagm
(** Converts the middleend format to the backend format *)

let new_let val' =
  let id = Kir.get_random_num () in
  ([ LetInVal (id, val') ], id)

let rec mtb_expr_h exp =
  match exp with
  | Kir.Val (_, i) -> new_let (Val i)
  | Kir.Int s -> new_let (Int s)
  | Kir.Float s -> new_let (Float s)
  | Kir.Str s -> new_let (String s)
  | Kir.Bool b -> new_let (Bool (string_of_bool b))
  | Kir.Tuple (_, t) ->
      let many = List.map mtb_expr_h t in
      let vals = List.concat @@ List.map fst many in
      let val', id =
        new_let (Tuple (List.map (fun x -> Val x) @@ List.map snd many))
      in
      let all = List.append vals val' in
      (all, id)
  | Kir.Call (_, e1, e2) ->
      let rec greedy_matchcall acc g =
        match g with
        | Kir.Call (_, e1, e2) -> greedy_matchcall (e2 :: acc) e1
        | _ -> g :: acc
      in
      let args = greedy_matchcall [] e1 @ [ e2 ] in
      let codeandids = List.map mtb_expr_h args in
      let code = List.concat @@ List.map fst codeandids in
      let ids = List.map snd codeandids in
      let id' = Kir.get_random_num () in
      let ours = LetInCall (id', List.map (fun x -> Val x) ids) in
      (code @ [ ours ], id')
  | Kir.Seq (_, e1, e2) ->
      let codes, id = mtb_expr_h e1 in
      let codes', id' = mtb_expr_h e2 in
      (codes @ codes', id')

let mtb_expr t =
  let code, id = mtb_expr_h t in
  code @ (Return id :: [])

let rec get_from_lams exp =
  match exp with
  | Kir.Lam (_, v, exp) ->
      let v', exp' = get_from_lams exp in
      (v :: v', exp')
  | _ -> ([], exp)

let mtb_top code =
  match code with
  | Kir.LetRec (_, id, exp) | Kir.Let (_, id, exp) ->
      let args, exp' = get_from_lams exp in
      Khagm.Let (id, args, mtb_expr exp')
  | Extern (_, arity, v, s) -> Khagm.Extern (v, arity, s)
  | Bind (a, b) -> Khagm.Let (a, [], [ Return b ])
  | Noop -> Khagm.Noop

let rec make_constr tbl (name, arity, tag) =
  Khagm.Ctor (fst @@ Kir.get_from_tbl name tbl, arity)

let mtb kir =
  let (table : Kir.kir_table), toplevel = kir in
  let ctors = table.constrs in
  let code' = ListHelpers.map mtb_top toplevel in
  let ctors' = ListHelpers.map (make_constr table) ctors in
  (ctors' @ code', table)
