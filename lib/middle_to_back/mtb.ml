open Exp

let kirval_to_khagmid x = x
let kv_to_kg = kirval_to_khagmid

let tuple_access i =
  Khagm.Call (Khagm.Val (-2), Khagm.Unboxed (Khagm.Int' (string_of_int i)))

let rec mtb_expr exp =
  match exp with
  | Kir.Val (_, v) -> Khagm.Val (kv_to_kg v)
  | Int s -> Khagm.Unboxed (Khagm.Int' s)
  | Float s -> Khagm.Unboxed (Khagm.Float' s)
  | Str s -> Khagm.Unboxed (Khagm.String' s)
  | Bool s -> Khagm.Unboxed (Khagm.Bool' s)
  | Tuple (_, e) -> Khagm.Tuple (List.map mtb_expr e)
  | Call (_, e1, e2) -> Khagm.Call (mtb_expr e1, mtb_expr e2)
  | Seq (_, e1, e2) -> Khagm.Seq (mtb_expr e1, mtb_expr e2)
  | TupAcc (_, e1, i) -> Khagm.Call (tuple_access i, mtb_expr e1)
  | Lam (_, _, _) -> raise @@ Impossible "Lambda in mtb"
  | Let (_, v, e1, e2) -> Khagm.Let (kv_to_kg v, mtb_expr e1, mtb_expr e2)
  | IfElse (_, e1, e2, e3) ->
      Khagm.IfElse (mtb_expr e1, mtb_expr e2, mtb_expr e3)

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
      Khagm.Let (kv_to_kg id, args, mtb_expr exp')
  | Extern (_, v, s) -> Khagm.Extern (kv_to_kg v, s)
  | Bind (a, b) -> Khagm.Let (a, [], Khagm.Val b)

let mtb kp =
  let typs, code = kp in
  let code' = List.map mtb_top code in
  (code', typs)
