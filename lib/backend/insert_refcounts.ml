open Exp
open Khagm

let refs ctx ls =
  ListHelpers.in_x_not_y ls ctx |> ListHelpers.map (fun x -> Ref x)

let unrefs ctx ls =
  ListHelpers.in_x_not_y ls ctx |> ListHelpers.map (fun x -> Unref x)

let rec from_val v =
  match v with
  | Val i -> [ i ]
  | Tuple l -> List.concat_map from_val l
  | _ -> []

let rec occur_shallow t =
  match t with
  | Fail _ -> []
  | LetInVal (i, vs) -> i :: from_val vs
  | LetInCall (i, func, args) -> i :: func :: args
  | Special (i, vs, _spec) -> i :: from_val vs
  | IfElse (i, cond, _e1, _e2) -> [ i; cond ]
  | SubExpr (i, _e) -> [ i ]
  | CheckCtor (ret, i, _index) -> [ ret; i ]
  | Return i -> [ i ]
  | Ref i -> [ i ]
  | Unref i -> [ i ]

let rec occur_deep t =
  match t with
  | Fail _ -> []
  | LetInVal (i, vs) -> i :: from_val vs
  | LetInCall (i, func, args) -> i :: func :: args
  | Special (i, vs, _spec) -> i :: from_val vs
  | IfElse (i, cond, e1, e2) ->
      [ i; cond ]
      @ List.concat_map occur_deep e1
      @ List.concat_map occur_deep e2
  | SubExpr (i, e) -> [ i ] @ List.concat_map occur_deep e
  | CheckCtor (ret, i, _index) -> [ ret; i ]
  | Return i -> [ i ]
  | Ref i -> [ i ]
  | Unref i -> [ i ]

let rec get_curr_unrefs ctx curr rest =
  let in_curr = occur_shallow curr in
  let in_rest = List.concat_map occur_deep rest in
  let diff = ListHelpers.in_x_not_y in_curr in_rest in
  unrefs ctx diff

and insert_expr ctx body =
  match body with
  | [] -> []
  | curr :: rest -> (
      match curr with
      | Return _ -> curr :: insert_expr ctx rest
      | IfElse (e, c, e1, e2) ->
          let e1' = insert_expr ctx e1 in
          let e2' = insert_expr ctx e2 in
          let if' = get_curr_unrefs ctx curr rest in
          (IfElse (e, c, e1', e2') :: if') @ insert_expr ctx rest
      | SubExpr (i, e) ->
          let e' = insert_expr ctx e in
          let expr = get_curr_unrefs ctx curr rest in
          (SubExpr (i, e') :: expr) @ insert_expr ctx rest
      | LetInCall (_ret, _func, args) ->
          let r = refs ctx args in
          let e = get_curr_unrefs ctx curr rest in
          r @ (curr :: e) @ insert_expr ctx rest
      | _ -> (curr :: get_curr_unrefs ctx curr rest) @ insert_expr ctx rest)

let insert_top ctx code =
  List.map
    (fun elm ->
      match elm with
      | Let (id, args, body) -> Let (id, args, insert_expr ctx body)
      | _ -> elm)
    code

let insert_refcounts (khagm : Khagm.khagm) =
  let code, tbl = khagm in
  let fst_binds = List.map fst tbl.binds in
  (insert_top fst_binds code, tbl)
