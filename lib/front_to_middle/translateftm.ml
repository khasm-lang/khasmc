open Exp

let rec fold_tup_flat f s l =
  match l with
  | [] -> raise @@ Impossible "empty fold_tup"
  | [ x ] ->
      let a, b = f s x in
      (a, b)
  | x :: xs ->
      let a, b = fold_tup_flat f s xs in
      let c, d = f b x in
      (c @ a, d)

let rec fold_tup f s l =
  match l with
  | [] -> raise @@ Impossible "empty fold_tup"
  | [ x ] ->
      let a, b = f s x in
      ([ a ], b)
  | x :: xs ->
      let a, b = fold_tup f s xs in
      let c, d = f b x in
      (c :: a, d)

let rec ftm_base (tbl : (int, string) Hashtbl.t ref) base =
  raise @@ Todo "ftm_base"

and ftm_expr tbl expr = raise @@ Todo "ftm_expr"

let rec ftm_toplevel table top prefix = raise @@ Todo "ftm_toplevel"

let rec ftm table prog =
  match prog with
  | Ast.Program tl ->
      fold_tup_flat (fun x y -> ftm_toplevel x y "") table (List.rev tl)

let front_to_middle proglist =
  let a, b = fold_tup ftm (Kir.empty_transtable ()) proglist in
  let a' = List.flatten a in
  (b, a')
