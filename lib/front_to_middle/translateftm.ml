open Exp

(** Converts between the frontend IR and the middleend IR. *)

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

module MatchMap = Map.Make (String)

let most_common tbl p =
  match p with
  | [] -> ([], [])
  | _ ->
      let empty = MatchMap.empty in
      let rec helper m x =
        match fst x with
        | Ast.MPInt _ -> m
        | Ast.MPId t ->
            if Kir.is_constr tbl t then
              match MatchMap.find_opt t m with
              | None -> MatchMap.add t 1 m
              | Some n -> MatchMap.add t (n + 1) m
            else
              m
        | Ast.MPApp (t, l) ->
            if Kir.is_constr tbl t then
              match MatchMap.find_opt t m with
              | None -> MatchMap.add t 1 m
              | Some n -> MatchMap.add t (n + 1) m
            else
              raise @@ Impossible "App not constr"
        | Ast.MPTup _ -> m
      in
      let main = List.fold_left helper empty p in
      let largest, _ =
        MatchMap.fold
          (fun key v i ->
            if v > snd i then
              (key, v)
            else
              i)
          main ("IMPOSSIBLE", -1)
      in
      let tmp =
        List.partition
          (fun x ->
            match fst x with
            | Ast.MPInt _ -> false
            | Ast.MPId _ -> false
            | Ast.MPApp (a, _) -> a = largest
            | Ast.MPTup _ -> false)
          p
      in
      if fst tmp = [] then
        (List.hd p :: [], List.tl p)
      else
        tmp

let rec compile_single tbl pat = todo "compile_single"

and compile_match_h tbl pat e =
  match pat with
  | [] -> raise @@ Impossible "compile_match_h empty"
  | [ x ] -> Kir.Switch (e, compile_single tbl x, None)
  | x :: xs ->
      let p = compile_single tbl x in
      Kir.Switch (e, p, Some (compile_match_h tbl xs e))

and match_compilation tbl pats expr =
  let rec h p =
    match most_common tbl p with [], [] -> [] | x, xs -> x :: h xs
  in
  let tmp = h pats in
  let tvar, tbl = Kir.new_var tbl in
  let pat' = compile_match_h tbl tmp tvar in
  let t = Ast.TSTuple [] in
  let swtch = Kir.SwitchConstr (t, Kir.Val (t, tvar), pat') in
  let l : Kir.kirexpr = Kir.Let (Kir.kirexpr_typ expr, tvar, expr, swtch) in
  l

and ftm_base tbl base =
  match base with
  | Ast.Ident (id, str) ->
      let asint, _str = Kir.get_from_tbl str tbl in
      Kir.Val (Hash.get_typ id.id, asint)
  | Ast.Int s -> Kir.Int s
  | Ast.Float s -> Kir.Float s
  | Ast.Str s -> Kir.Str s
  | Ast.Tuple l ->
      let have = List.map (ftm_expr tbl) l in
      let typ = Ast.TSTuple (List.map Kir.kirexpr_typ have) in
      Kir.Tuple (typ, have)
  | Ast.True -> Kir.Bool true
  | Ast.False -> Kir.Bool false

and ftm_expr tbl expr =
  match expr with
  | Ast.Base (_, b) -> ftm_base tbl b
  | Ast.FCall (id, a, b) ->
      Kir.Call (Hash.get_typ id.id, ftm_expr tbl a, ftm_expr tbl b)
  | Ast.AnnotLet (id, a, _, e1, e2) | Ast.LetIn (id, a, e1, e2) ->
      let id', tbl' = Kir.add_to_tbl a tbl in
      let b1 = ftm_expr tbl e1 in
      let b2 = ftm_expr tbl' e2 in
      Kir.Let (Hash.get_typ id.id, id', b1, b2)
  | Ast.LetRecIn (info, _ts, id, e1, e2) ->
      let id', tbl' = Kir.add_to_tbl id tbl in
      let b1 = ftm_expr tbl' e1 in
      let b2 = ftm_expr tbl' e2 in
      Kir.Let (Hash.get_typ info.id, id', b1, b2)
  | Ast.AnnotLam (_, a, _, e) | Ast.Lam (_, a, e) ->
      let id', tbl' = Kir.add_to_tbl a tbl in
      let b = ftm_expr tbl' e in
      let typ = Kir.kirexpr_typ b in
      Kir.Lam (typ, id', b)
  | Ast.IfElse (id, c, e1, e2) ->
      let typ = Hash.get_typ id.id in
      let c' = ftm_expr tbl c in
      let e1' = ftm_expr tbl e1 in
      let e2' = ftm_expr tbl e2 in
      Kir.IfElse (typ, c', e1', e2')
  | Ast.Join (id, e1, e2) ->
      let typ = Hash.get_typ id.id in
      let e1 = ftm_expr tbl e1 in
      let e2 = ftm_expr tbl e2 in
      Kir.Seq (typ, e1, e2)
  | Ast.Inst (_, _, _) -> raise @@ Impossible "INST"
  | Ast.TypeLam (_, _, e) -> ftm_expr tbl e
  | Ast.TupAccess (id, expr, i) ->
      let typ = Hash.get_typ id.id in
      let e' = ftm_expr tbl expr in
      Kir.TupAcc (typ, e', i)
  | Ast.Match (id, expr, i) ->
      print_endline (Kir.show_kir_table tbl);
      let matchee = ftm_expr tbl expr in
      let patterns = match_compilation tbl i matchee in
      patterns
  | Ast.ModAccess (_, _, _) -> raise @@ Impossible "Modules in middleend"

let rec ftm_toplevel table top =
  match top with
  | Ast.TopAssign ((id, ts), (_id, args, expr)) ->
      let body = Typecheck.conv_ts_args_body_to_typelams ts args expr in
      let id', tbl' = Kir.add_to_tbl id table in
      (Kir.Let (ts, id', ftm_expr table body), tbl')
  | Ast.TopAssignRec ((id, ts), (_id, args, expr)) ->
      let body = Typecheck.conv_ts_args_body_to_typelams ts args expr in
      let id', tbl' = Kir.add_to_tbl id table in
      (Kir.LetRec (ts, id', ftm_expr tbl' body), tbl')
  | Ast.Extern (id, arity, ts) ->
      let id', tbl' = Kir.add_to_tbl id table in
      (Kir.Extern (ts, arity, id', id), tbl')
  | Ast.Bind (id, _, nm) ->
      let id', tbl' = Kir.add_to_tbl id table in
      let v, _name = Kir.get_from_tbl nm tbl' in
      (Kir.Bind (id', v), tbl')
  | Ast.IntExtern (id, id', arity, ts) ->
      let _id1, tbl' = Kir.add_to_tbl id table in
      let id2, tbl'' = Kir.add_to_tbl id' tbl' in
      (Kir.Extern (ts, arity, id2, id), tbl'')
  | Ast.Typealias (_, _, _) -> (Kir.Noop, table)
  | Ast.Typedecl (_nm, _args, pats) ->
      let count = ref (-1) in
      let table' =
        List.fold_left
          (fun t (p : Ast.adt_pattern) ->
            count := !count + 1;
            let m = Kir.add_constr t p.head (List.length p.args) !count in
            snd @@ Kir.add_to_tbl p.head m)
          table pats
      in
      (Kir.Noop, table')
  | Ast.Open _ | Ast.SimplModule (_, _) -> raise @@ Impossible "Modules in ftm"

let rec ftm table prog =
  match prog with
  | Ast.Program [] -> ([], table)
  | Ast.Program tl ->
      let a, b = fold_tup (fun x y -> ftm_toplevel x y) table (List.rev tl) in
      (a, b)

let front_to_middle proglist =
  let a, b = fold_tup ftm (Kir.empty_transtable ()) (List.rev proglist) in
  let a' = List.rev @@ List.flatten a in
  (b, a')
