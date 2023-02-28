open Exp
open Kir

type lamctx = { frees : (kirval * kirtype) list }
[@@deriving show { with_path = false }]

let emptyctx () = { frees = [] }
let add_bound ctx v ts = { ctx with frees = (v, ts) :: ctx.frees }
let ctxwith frees = { frees }

let rec deconstruct_assoc_list al =
  match al with
  | [] -> ([], [])
  | [ x ] -> ([ fst x ], [ snd x ])
  | x :: xs ->
      let a, b = deconstruct_assoc_list [ x ] in
      let c, d = deconstruct_assoc_list xs in
      (a @ c, b @ d)

let rec gen_lams ctx'frees inner =
  match ctx'frees with
  | [] -> ([], inner)
  | x :: xs ->
      let v, ts = x in
      let a, b = gen_lams xs inner in
      print_endline "loop?";
      print_endline (show_kirexpr b);
      let c, d = llift_expr (ctxwith ctx'frees) b in
      (a @ c, d)

and gen_fcall ctx'frees inner =
  match ctx'frees with
  | [] -> inner
  | x :: xs ->
      let v, ts = x in
      let t = gen_fcall xs inner in
      Call (kirexpr_typ t, t, Val (ts, v))

and llift_expr ctx expr =
  print_endline "\n\nDEBUG:";
  print_endline (show_lamctx ctx);
  print_endline (show_kirexpr expr);
  print_endline "DONE;\n";
  match expr with
  | Val (_, _) | Int _ | Float _ | Str _ | Bool _ -> ([], expr)
  | Tuple (ts, expr) ->
      let tmp = List.map (llift_expr ctx) expr in
      let a, b = deconstruct_assoc_list tmp in
      (List.flatten a, Tuple (ts, b))
  | Call (ts, e1, e2) ->
      let a, b = llift_expr ctx e1 in
      let c, d = llift_expr ctx e2 in
      (a @ c, Call (ts, b, d))
  | Seq (ts, e1, e2) ->
      let a, b = llift_expr ctx e1 in
      let c, d = llift_expr ctx e2 in
      (a @ c, Seq (ts, b, d))
  | TupAcc (ts, ex, i) ->
      let a, b = llift_expr ctx ex in
      (a, TupAcc (ts, b, i))
  | Let (ts, v, e1, e2) ->
      let ctx' = add_bound ctx v (kirexpr_typ e1) in
      let a, b = llift_expr ctx e1 in
      let c, d = llift_expr ctx' e2 in
      (a @ c, Let (ts, v, b, d))
  | Lam (ts, v, e) ->
      let ctx' = add_bound ctx v ts in
      let added1, e' = llift_expr ctx' e in
      print_endline "huh?";
      print_endline (show_kirexpr e');
      let added2, get = gen_lams ctx.frees e' in
      let final = Let (ts, v, get) in
      let asval = Val (ts, v) in
      let call = gen_fcall ctx.frees asval in
      print_endline "weird?";
      print_endline (show_kirexpr call);
      ((final :: added1) @ added2, call)
  | IfElse (ts, e1, e2, e3) ->
      let a, b = llift_expr ctx e1 in
      let c, d = llift_expr ctx e2 in
      let e, f = llift_expr ctx e3 in
      (a @ c @ e, IfElse (ts, b, d, f))

let rec llift_top top =
  match top with
  | Extern (_, _, _) -> ([], top)
  | Bind (_, _) -> ([], top)
  | Let (ts, v, exp) ->
      let added, n = llift_expr (emptyctx ()) exp in
      (added, Let (ts, v, n))
  | LetRec (ts, v, exp) ->
      let added, n = llift_expr (emptyctx ()) exp in
      (added, LetRec (ts, v, n))

let rec lambda_lift_h tops =
  match tops with
  | [] -> []
  | x :: xs ->
      let added, n = llift_top x in
      (added @ (n :: [])) @ lambda_lift_h xs

let lambda_lift kir = (fst kir, lambda_lift_h @@ snd kir)
