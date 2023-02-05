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
  match base with
  | Ast.Ident (info, str) ->
      let id =
        match Kir.get_from_tbl str tbl with
        | Some s -> s
        | None -> Kir.add_to_tbl str tbl
      in
      Kir.Val (Hash.get_typ info.id, id)
  | Ast.Int s -> Kir.Int s

and ftm_expr tbl expr =
  match expr with Ast.Base (_, kbase) -> ftm_base tbl kbase

let rec ftm_toplevel table top prefix =
  match top with
  | Ast.TopAssign ((id, ts), (_, args, body)) ->
      let body' = Typecheck.conv_ts_args_body_to_typelams ts args body in
      let id = Kir.add_to_tbl id table in
      ([ Kir.Let (ts, id, ftm_expr (Kir.empty_transtable ()) body') ], table)
  | Ast.SimplModule (id, top') ->
      fold_tup_flat (fun x y -> ftm_toplevel x y (id ^ ".")) table top'
  | x ->
      print_endline (Ast.show_toplevel x);
      raise @@ Impossible "huh"

let rec ftm table prog =
  match prog with
  | Ast.Program tl -> fold_tup_flat (fun x y -> ftm_toplevel x y "") table tl

let front_to_middle proglist =
  let a, b = fold_tup ftm (Kir.empty_transtable ()) proglist in
  let a' = List.flatten a in
  (b, a')
