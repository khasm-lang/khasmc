open Exp

open Khagm
(** Converts the middleend format to the backend format *)

let new_let val' =
  let id = Kir.get_random_num () in
  ([ LetInVal (id, val') ], id)

let rec mtb_matchtree mt =
  match mt with
  | Kir.Success e -> mtb_expr e
  | Kir.Failure -> [ Fail "Match failure" ]
  | Kir.Switch (e, case, mt1, mt2) -> (
      match case with
      | Wildcard | BindTuple ->
          let e' = mtb_expr e in
          let code = mtb_matchtree mt1 in
          e' @ code
      | BindCtor i ->
          let ret = Kir.get_random_num () in
          let tmp = Kir.get_random_num () in
          let expr, exprid = mtb_expr_h e in
          let cond = CheckCtor (tmp, exprid, i) in
          let case1code = mtb_matchtree mt1 in
          let case2code = mtb_matchtree mt2 in
          let if' = IfElse (ret, tmp, case1code, case2code) in
          expr @ [ cond; if'; Return ret ])

and mtb_expr_h exp =
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
  | Kir.Call (_, e1, e2) -> (
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
      match ids with
      | [ x; y ] ->
          let ours = LetInCall (id', x, [ y ]) in
          (code @ [ ours ], id')
      | x :: xs ->
          let ours = LetInCall (id', x, xs) in
          (code @ [ ours ], id')
      | _ -> impossible "zero args to function call in mtb")
  | Kir.Seq (_, e1, e2) ->
      let codes, _ = mtb_expr_h e1 in
      let codes', id' = mtb_expr_h e2 in
      (codes @ codes', id')
  | Kir.IfElse (_, c, e1, e2) ->
      let condcode, condid = mtb_expr_h c in
      let e1code = mtb_expr e1 in
      let e2code = mtb_expr e2 in
      let id = Kir.get_random_num () in
      let ifelse = IfElse (id, condid, e1code, e2code) in
      (condcode @ [ ifelse ], id)
  | Kir.TupAcc (_, e, i) ->
      let code, e' = mtb_expr_h e in
      let id = Kir.get_random_num () in
      (code @ [ Special (id, Val e', TupAcc i) ], id)
  | Kir.Let (_, id, exp1, exp2) ->
      let code1, expr1 = mtb_expr_h exp1 in
      let l = LetInVal (id, Val expr1) in
      let code2, expr2 = mtb_expr_h exp2 in
      (code1 @ (l :: code2), expr2)
  | Kir.SwitchConstr (_, e, mt) ->
      let code, _id = mtb_expr_h e in
      let mtcode = mtb_matchtree mt in
      let ret = Kir.get_random_num () in
      let sub = SubExpr (ret, mtcode) in
      (code @ [ sub ], ret)
  | Kir.Fail s -> ([ Fail s ], -1)
  | Kir.Lam (_, _, _) -> impossible "lam in translation mtb"

and mtb_expr t =
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
