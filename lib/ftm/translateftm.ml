open Exp
open Either

open ListHelpers
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

type patmatrix = {
  inputs : Kir.kirexpr list;
  body : Ast.matchpat list list;
  outputs : (Ast.kexpr, Kir.kirexpr) result list;
}
[@@deriving show { with_path = false }]

let print_matrix m =
  List.iter
    (fun y ->
      List.iter
        (fun x ->
          print_string (Ast.show_matchpat x);
          print_string ", ")
        y;
      print_endline "")
    m

let rec crashme () = crashme ()

and gen_first_patmatrix pats exprs tmpvar =
  let open Ast in
  let body =
    List.map
      (fun pat ->
        match pat with
        | MPApp (i, l) ->
            if List.length l >= 1 then
              [ pat ]
            else
              [ MPApp (i, [ MPWild ]) ]
        | MPId _ -> [ pat ]
        | MPInt _ -> [ pat ]
        | MPWild -> [ pat ]
        | MPTup t -> t)
      pats
  in
  let goods = List.filter (fun x -> List.length x > 1) body in
  match goods with
  | [] ->
      (* no tuples at all, v. cool *)
      { inputs = [ tmpvar ]; body; outputs = exprs }
  | x :: _ ->
      let rec goh e n =
        if n = -1 then
          []
        else
          Kir.TupAcc (TSTuple [], tmpvar, n) :: goh e (n - 1)
      in
      let go e n = List.rev (goh e (n - 1)) in
      let inputs = go tmpvar (List.length x) in
      { inputs; body; outputs = exprs }

and gen_accesses_h l e n =
  let open Ast in
  match l with
  | [] -> []
  | _ :: xs -> Kir.TupAcc (TSTuple [], e, n) :: gen_accesses_h xs e (n - 1)

and gen_accesses l e = List.rev @@ gen_accesses_h l e (List.length l - 1)

and find_useful_h row =
  let rec go row acc =
    let open Ast in
    match row with
    | [] -> None
    | x :: xs -> (
        match x with MPApp (_, _) -> Some (acc, x) | _ -> go xs (acc + 1))
  in
  go row 0

and find_useful row inputs = (row, inputs, 0)

and find_useful_matrix matrix =
  match matrix with
  | [] -> None
  | x :: xs -> (
      match find_useful_h x with
      | Some n -> Some n
      | None -> find_useful_matrix xs)

and push_bind_into_expr tbl ident kirexpr kexpr =
  match kexpr with
  | Ok t ->
      let gen = ftm_expr tbl t in
      let id, _ = Kir.get_from_tbl ident tbl in
      let l : Kir.kirexpr = Kir.Let (Ast.TSTuple [], id, kirexpr, gen) in
      l
  | Error t ->
      let id, _ = Kir.get_from_tbl ident tbl in
      let l : Kir.kirexpr = Kir.Let (Ast.TSTuple [], id, kirexpr, t) in
      l

and split_on head body outputs =
  let open Ast in
  let open List in
  let comp i (pat, expr) =
    match List.hd pat with
    | MPApp (q, _) ->
        if i = q then
          ListHelpers.True
        else
          False
    | _ -> Both
  in
  match head with
  | MPApp (i, _) ->
      let left, right =
        List.combine body outputs |> partition_three (fun x -> comp i x)
      in
      let l, le = List.split left in
      let r, re = List.split right in
      ((l, le), (r, re))
  | _ -> (
      match (body, outputs) with
      | [], [] -> todo "empty body and outputs?"
      | x :: xs, y :: ys -> (([ x ], [ y ]), (xs, ys))
      | _, _ -> impossible "not-equal body and outputs length")

and split_patmatrix ?(again = false) tbl pat =
  let open Ast in
  try
    match pat.body with
    | [] -> todo "empty body?"
    | [ x ] ->
        let tbl =
          if not again then
            let frees = List.concat_map Ast.get_pat_frees x in
            List.fold_left (fun acc x -> snd (Kir.add_to_tbl x acc)) tbl frees
          else
            tbl
        in
        begin
          let first :: rest, inputs, _index = find_useful x pat.inputs in
          match first with
          | MPApp (id, bd) ->
              let case =
                Kir.BindCtor
                  (let (Some (_, _, a)) = Kir.get_constr tbl id in
                   a)
              in
              let newins = gen_accesses bd (List.hd inputs) @ List.tl inputs in
              let new' =
                { outputs = pat.outputs; body = [ bd @ rest ]; inputs = newins }
              in
              (tbl, Right (Some (case, List.hd inputs), new'))
          | MPTup bd ->
              let case = Kir.BindTuple in
              let newins = gen_accesses bd (List.hd inputs) @ List.tl inputs in
              let new' =
                { outputs = pat.outputs; body = [ bd @ rest ]; inputs = newins }
              in
              (tbl, Right (Some (case, List.hd inputs), new'))
          | MPId i ->
              let (curr :: inputrest) = inputs in
              let (expr :: exprrest) = pat.outputs in
              let expr = push_bind_into_expr tbl i curr expr in
              let new' =
                {
                  outputs = Error expr :: exprrest;
                  body = [ MPWild :: rest ];
                  inputs = curr :: inputrest;
                }
              in
              (tbl, Right (None, new'))
          | MPInt _t -> todo "MPInt"
          | MPWild ->
              let (curr :: inputrest) = inputs in
              let new' =
                { outputs = pat.outputs; body = [ rest ]; inputs = inputrest }
              in
              (tbl, Right (Some (Kir.Wildcard, curr), new'))
        end
        [@warning "-8"]
    | _ :: _ -> (
        match find_useful_matrix pat.body with
        | None -> todo "no useful stuff"
        | Some (index, useful) ->
            let inputs = ListHelpers.make_head pat.inputs index in
            let body =
              List.map (fun x -> ListHelpers.make_head x index) pat.body
            in
            let (left, leftouts), (right, rightouts) =
              split_on useful body pat.outputs
            in
            let left'pat = { inputs; body = left; outputs = leftouts } in
            let right'pat = { inputs; body = right; outputs = rightouts } in
            let case =
              match useful with
              | MPApp (i, _) ->
                  Kir.BindCtor
                    (let[@warning "-8"] (Some (_, _, a)) =
                       Kir.get_constr tbl i
                     in
                     a)
              | MPTup _ -> Kir.BindTuple
              | _ -> todo "other stuff here?"
            in
            ( tbl,
              Left
                ( Some (case, List.hd (ListHelpers.make_head pat.inputs index)),
                  left'pat,
                  right'pat ) ))
  with Match_failure (a, b, c) ->
    impossible @@ "Match failure in pattern matching compl split" ^ a ^ " "
    ^ string_of_int b ^ " " ^ string_of_int c

and subproblem ?(aroundagain = false) tbl patmatrix =
  let open Ast in
  match split_patmatrix tbl patmatrix ~again:aroundagain with
  | tbl, Left (caseandexpr, one, two) -> (
      match caseandexpr with
      | None -> todo "caseandexpr blank on return from multi?"
      | Some (case, expr) ->
          let left = subproblem tbl one ~aroundagain in
          let right = subproblem tbl two ~aroundagain in
          Kir.Switch (expr, case, left, right))
  | tbl, Right (caseandexpr, one) -> (
      match one.body with
      | [] :: _ :: _ | [] -> impossible "empty body"
      | [ [] ] -> (
          match one.outputs with
          | [ x ] -> (
              match x with
              | Ok t -> Kir.Success (ftm_expr tbl t)
              | Error t -> Kir.Success t)
          | _ :: _ | [] -> impossible "bad output?")
      | (_ :: _) :: _ -> (
          let sub = subproblem tbl one ~aroundagain:true in
          match caseandexpr with
          | None -> sub
          | Some (case, expr) -> Kir.Switch (expr, case, sub, Kir.Failure)))

and match_compilation tbl (pats : (Ast.matchpat * Ast.kexpr) list)
    (matchee : Kir.kirexpr) =
  let pats, exprs = List.split pats in
  let tid, tbl = Kir.new_var tbl in
  let tmpvar = Kir.Val (TSTuple [], tid) in
  let firstpat =
    gen_first_patmatrix pats (List.map (fun x -> Ok x) exprs) tmpvar
  in
  let result = subproblem tbl firstpat in
  let swch =
    Kir.SwitchConstr (Ast.TSTuple [], Kir.Val (Ast.TSTuple [], tid), result)
  in
  let l : Kir.kirexpr = Kir.Let (Ast.TSTuple [], tid, matchee, swch) in
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
      let matchee = ftm_expr tbl expr in
      let m = match_compilation tbl i matchee in
      m
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
