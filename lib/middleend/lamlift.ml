open Exp
open Kir

let rec deconstruct_assoc_list al =
  match al with
  | [] -> ([], [])
  | [ x ] -> ([ fst x ], [ snd x ])
  | x :: xs ->
      let a, b = deconstruct_assoc_list [ x ] in
      let c, d = deconstruct_assoc_list xs in
      (a @ c, b @ d)

let rec llift_expr expr =
  match expr with
  | Val (_, _) | Int _ | Float _ | Str _ | Bool _ -> ([], expr)
  | Tuple (ts, expr) ->
      let tmp = List.map llift_expr expr in
      let a, b = deconstruct_assoc_list tmp in
      (List.flatten a, Tuple (ts, b))
  | Call (ts, e1, e2) ->
      let a, b = llift_expr e1 in
      let c, d = llift_expr e2 in
      (a @ c, Call (ts, b, d))
  | Seq (ts, e1, e2) ->
      let a, b = llift_expr e1 in
      let c, d = llift_expr e2 in
      (a @ c, Seq (ts, b, d))
  | TupAcc (ts, ex, i) ->
      let a, b = llift_expr ex in
      (a, TupAcc (ts, b, i))

let rec llift_top top =
  match top with
  | Extern (_, _, _) -> ([], top)
  | Bind (_, _) -> ([], top)
  | Let (ts, v, exp) ->
      let added, n = llift_expr exp in
      (added, Let (ts, v, n))

let rec lambda_lift tops =
  match tops with
  | [] -> []
  | x :: xs ->
      let added, n = llift_top x in
      (added @ (n :: [])) :: lambda_lift xs
