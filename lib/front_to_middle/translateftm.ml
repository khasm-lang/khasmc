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

let most_common xs =
  if xs = [] then
    raise @@ NotFound "most_common empty";
  let tbl =
    let tbl' = Hashtbl.create 16 in
    List.iter
      (fun n ->
        match Hashtbl.find_opt tbl' n with
        | None -> Hashtbl.add tbl' n 0
        | Some x -> Hashtbl.add tbl' n (x + 1))
      xs;
    tbl'
  in
  Hashtbl.fold
    (fun k v curr ->
      if v > match Hashtbl.find_opt tbl curr with None -> -1 | Some n -> n
      then
        k
      else
        curr)
    tbl (List.hd xs)

let more_interesting e1 e2 =
  let open Ast in
  match (e1, e2) with
  | _, MPTup _ -> e2
  | _, MPApp (_, _) -> e2
  | _, MPInt _ -> e2
  | _, _ -> e1

let rec frees e1 =
  let open Ast in
  match e1 with
  | MPId a -> [ a ]
  | MPApp (_, l) -> List.concat_map frees l
  | MPTup l -> List.concat_map frees l
  | MPWild -> []
  | MPInt _ -> []

let rec most_interesting curr ps =
  match ps with
  | [] -> impossible "empty pats most interesting"
  | [ x ] -> more_interesting curr x
  | x :: xs -> more_interesting (most_interesting curr xs) x

let rec most_interesting_i curr ps =
  let t = most_interesting curr ps in
  let mb = ListHelpers.indexof ps t in
  match mb with None -> (0, t) | Some x -> (x, t)

let rec to_matrix _tbl expr pats =
  let open Ast in
  let ps = List.map fst pats in
  let expr_column = List.map snd pats in
  let input_row, pat_tbl = pat_to_matrix expr ps in
  (input_row, pat_tbl, expr_column)

and pat_to_matrix expr pats =
  let open Ast in
  match pats with
  | [] -> impossible "empty pats"
  | _ ->
      let matrix =
        List.map
          (fun x ->
            match x with
            | MPApp (s, c) ->
                if c = [] then
                  MPApp (s, [ MPWild ]) :: []
                else
                  MPApp (s, c) :: []
            | MPId i -> MPId i :: []
            | MPInt i -> MPInt i :: []
            | MPTup t -> t
            | MPWild -> MPWild :: [])
          pats
      in
      let mi = most_interesting MPWild pats in
      let expr =
        match mi with
        | MPWild | MPId _ | MPApp _ | MPInt _ -> [ expr ]
        | MPTup t -> List.mapi (fun i _x -> Kir.TupAcc (TSTuple [], expr, i)) t
      in
      (expr, matrix)

and partition most pats res = todo "partition"

and subprogram tbl input pats result =
  let open Ast in
  let interestings = List.map (most_interesting_i MPWild) pats in
  let mostid, most =
    match interestings with
    | [] -> impossible "nothing interesting"
    | x :: _ -> x
  in
  print_endline (string_of_int mostid);
  print_endline (show_matchpat most);
  let pats' = List.map (fun x -> ListHelpers.make_head x mostid) pats in
  let input = ListHelpers.make_head input mostid in
  let (goods, goodouts), (bads, badouts) = partition most pats res in
  todo "bad good?"

and match_compilation tbl pats (expr : Kir.kirexpr) =
  let open Ast in
  let newvar, tbl = Kir.new_var tbl in
  let newvar = Kir.Val (TSTuple [], newvar) in
  let inp, pats, res = to_matrix tbl expr pats in
  let tmp = subprogram tbl inp pats res in
  todo "matchcomp"

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
      todo "what goes here"
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
