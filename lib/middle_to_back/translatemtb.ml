open Helpers.Exp
open Middleend
open Backend

(* Converts the middleend format to the backend format *)
open Helpers

let kirval_to_khagmid x = x
let kv_to_kg = kirval_to_khagmid

let tuple_access i =
  Khagm.Call (Khagm.Val (-2), Khagm.Unboxed (Khagm.Int' (string_of_int i)))

let rec mtb_swchtree exp =
  match exp with
  | Kir.Failure -> Khagm.Fail "Match failure"
  | Kir.Success x -> mtb_expr x
  | Kir.Switch (e, case, tree1, tree2) -> (
      match case with
      | Kir.Wildcard | Kir.BindTuple -> mtb_swchtree tree1
      | Kir.BindCtor i ->
          Khagm.IfElse
            ( Khagm.CheckConstr (i, mtb_expr e),
              mtb_swchtree tree1,
              mtb_swchtree tree2 ))

and mtb_expr exp =
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
  | SwitchConstr (_, _var, tree) -> mtb_swchtree tree

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
  | Extern (_, arity, v, s) -> Khagm.Extern (kv_to_kg v, arity, s)
  | Bind (a, b) -> Khagm.Let (a, [], Khagm.Val b)
  | Noop -> Khagm.Noop

let rec make_constr tbl (name, arity, d) =
  let make_tup arity =
    let rec args n =
      if n = -1000 then
        []
      else
        n :: args (n + 1)
    in
    let a = args (-arity - 1000) in
    let b = List.map (fun x -> Khagm.Val x) a in
    (a, Khagm.Ctor (arity, d, b))
  in
  let args, body = make_tup arity in
  Khagm.Let (fst @@ Kir.get_from_tbl name tbl, args, body)

let mtb kp =
  let (typs : Kir.kir_table), code = kp in
  let constrs = typs.constrs in
  let ctors = ListHelpers.map (make_constr typs) constrs in
  let code' = ListHelpers.map mtb_top code in
  (ctors @ code', typs)
