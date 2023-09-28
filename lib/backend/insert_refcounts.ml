open Exp
open Khagm

(** Add refcounts to generated code *)

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
  | LetInUnboxCall (i, func, args) -> i :: func :: args
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
  | LetInUnboxCall (i, func, args) -> i :: func :: args
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

let refs ctx l =
  List.filter (fun x -> not @@ List.mem x ctx) l |> List.map (fun x -> Ref x)

let unrefs ctx l =
  List.filter (fun x -> not @@ List.mem x ctx) l |> List.map (fun x -> Unref x)

let rec most ctx curr rest =
  let c = occur_shallow curr in
  let m = List.concat_map occur_deep rest in
  unrefs ctx (ListHelpers.in_x_not_y c m)

and insert_expr ctx body =
  match body with
  | [] -> []
  | curr :: rest -> (
      match curr with
      | Return i -> (
          match rest with
          | [] -> [ Return i ]
          | _ ->
              List.iter (fun x -> print_endline @@ show_khagmexpr x) rest;
              impossible "return with stuff after it")
      | IfElse (ret, cond, e1, e2) ->
          let e1' = insert_expr ctx e1 in
          let e2' = insert_expr ctx e2 in
          let m = most ctx curr rest in
          (IfElse (ret, cond, m @ e1', m @ e2') :: []) @ insert_expr ctx rest
      | SubExpr (ret, e) ->
          let e' = insert_expr ctx e in
          let m = most ctx curr rest in
          (SubExpr (ret, e') :: m) @ insert_expr ctx rest
      | _ -> (curr :: most ctx curr rest) @ insert_expr ctx rest)

let insert_top ctx code =
  List.map
    (fun elm ->
      match elm with
      | Let (id, args, body) ->
          Let (id, args, insert_expr (args @ ctx) body @ unrefs ctx args)
      | _ -> elm)
    code

let insert_refcounts (khagm : Khagm.khagm) =
  let code, tbl = khagm in
  let fst_binds = List.map fst tbl.binds in
  (insert_top fst_binds code, tbl)
