open Parsing.Ast
open Typecheck
open Share.Maybe
open Share.Uuid

let partition_mapi (map : int -> 'a -> ('b, 'c) Either.t)
    (list : 'a list) : 'b list * 'c list =
  let rec go i l =
    match l with
    | [] -> ([], [])
    | x :: xs -> (
        let curr = map i x in
        let l, r = go (i + 1) xs in
        match curr with
        | Either.Left k -> (k :: l, r)
        | Either.Right k -> (l, k :: r))
  in
  go 0 list

(* accessors, rows
   match _ with
   (1, 2) ->
   (3, 4) ->
   end
   ==
   [[1,2],[3,4]]
   ,
   bodies
*)
type ('a, 'b) matrix =
  ('a, 'b) expr list * 'a case list list * ('a, 'b) expr list
[@@deriving show { with_path = false }]

let show_matrix' = show_matrix pp_resolved (pp_typ pp_resolved)
let fst3 (a, b, c) = a
let snd3 (a, b, c) = b
let trd3 (a, b, c) = c
let data_bot () = data_of @@ uuid_using TyBottom
let add_get_contr n e = UnaryOp (data_bot (), GetConstrField n, e)
let add_get_tuple n e = UnaryOp (data_bot (), Project n, e)

let rec is_pat_refutable (pat : 'a case) =
  match pat with
  | CaseWild -> false
  | CaseVar _ -> false
  | CaseTuple t -> List.exists is_pat_refutable t
  | CaseCtor (_, _) -> true
  | CaseLit _ -> true

let is_col_refutable n (matrix : ('a, 'b) matrix) =
  let acc, row, bodys = matrix in
  try
    row
    |> List.map (fun r -> List.nth r n)
    |> List.exists is_pat_refutable
  with _ ->
    print_endline "is_col_refutable BAD";
    print_endline (show_matrix' matrix);
    failwith "nth on incompat matrix?"

let swap_to_fst n list =
  if n = 0 then
    list
  else
    let fst = List.hd list in
    let top = List.nth list n in
    let rec go k rest =
      match rest with
      | [] -> failwith "empty rest?"
      | x :: xs ->
          if n = k then
            fst :: xs
          else
            x :: go (k + 1) xs
    in
    top :: go 1 (List.tl list)

let swap_to n (matrix : ('a, 'b) matrix) : ('a, 'b) matrix =
  let accs, rows, bodys = matrix in
  (swap_to_fst n accs, List.map (swap_to_fst n) rows, bodys)

let swap_to_refutable_if_exists (matrix : ('a, 'b) matrix) :
    ('a, 'b) matrix =
  let len = List.length (snd3 matrix) in
  let rec go n =
    if n = len then
      matrix
    else if is_col_refutable n matrix then
      swap_to n matrix
    else
      go (n + 1)
  in
  go 0

let rec normalize_cases (cases : ('a case * ('a, 'b) expr) list) =
  match
    List.find_map
      (fun (cs, epx) ->
        match cs with
        | CaseWild -> None
        | CaseVar _ -> None
        | CaseLit _ -> None
        | CaseCtor _ -> None
        | CaseTuple t -> Some (List.length t))
      cases
  with
  | None ->
      List.map
        (function
          | CaseWild, bd ->
              let f = fresh_resolved () in
              (CaseVar f, bd)
          | x -> x)
        cases
  | Some len ->
      let rec norm (cs : 'a case) exp =
        match cs with
        | CaseWild -> norm (CaseVar (fresh_resolved ())) exp
        | CaseVar v ->
            let freshs = n_fresh_resolved len in
            ( CaseTuple (List.map (fun x -> CaseVar x) freshs),
              LetIn
                ( data_bot (),
                  CaseVar v,
                  None,
                  Tuple
                    ( data_bot (),
                      List.map (fun x -> Var (data_bot (), x)) freshs
                    ),
                  exp ) )
        | otherwise -> (otherwise, exp)
      in
      List.map (fun (a, b) -> norm a b) cases

let normalize_first_col (matrix : ('a, 'b) matrix) =
  let accs, rows, bodys = matrix in
  if List.length rows = 0 then
    matrix
  else
    let fsts = List.map List.hd rows in
    let fsts', bodys' =
      List.split @@ normalize_cases (List.combine fsts bodys)
    in
    ( accs,
      List.map2 (fun col1 rest -> col1 :: List.tl rest) fsts' rows,
      bodys' )

let rec compile (matrix : ('a, 'b) matrix) : 'c =
  print_endline "\ncompiling matrix:";
  print_endline (show_matrix' matrix);
  print_endline "godspeed\n";
  let accs, rows, bodys = matrix in
  (* default out if patterns are empty
     TODO: add warning for when not everything is empty
  *)
  if List.exists (fun inner -> List.length inner = 0) rows then
    List.hd bodys
  else if List.length rows = 0 then
    Fail (data_bot (), "pattern matching failure")
  else
    let accs, rows, bodys = swap_to_refutable_if_exists matrix in
    let accs, rows, bodys = normalize_first_col (accs, rows, bodys) in
    (* just in case, one might say *)
    let fst_row = List.hd rows in
    match List.hd fst_row with
    (* impossible *)
    | CaseWild -> failwith "CaseWild"
    | CaseVar nm ->
        (* whelp we're taking this path *)
        let matrix' : ('a, 'b) matrix =
          ( List.tl accs,
            [ List.tl fst_row ],
            [
              ( List.hd bodys |> fun exp ->
                LetIn
                  (data_bot (), CaseVar nm, None, List.hd accs, exp)
              );
            ] )
        in
        compile matrix'
    | CaseTuple tup ->
        (* ughhhh ok gotta expand out another tuple *)
        let len = List.length tup in
        let added_accs =
          let hd = List.hd accs in
          let rec go n =
            if n = len then
              []
            else
              add_get_tuple n hd :: go (n + 1)
          in
          go 0
        in
        let matrix' =
          ( added_accs @ List.tl accs,
            List.map
              (function
                | CaseTuple tup -> tup
                | _ -> failwith "pattern compile tuple")
              fst_row
            @ List.tl rows,
            bodys )
        in
        compile matrix'
    | CaseCtor (ctorname, ctorbody) ->
        (* alrighty. we must:
       - gather constructors
       - make sure the arguments work themselves out correctly
       - generate submatrixes properly
       - try not to explode
    *)
        let yes, no =
          partition_mapi
            (fun idx elm ->
              let bod = List.nth bodys idx in
              (* elm is a row *)
              match List.hd elm with
              | CaseCtor (nm, rest) when nm = ctorname ->
                  Either.Left (rest @ List.tl elm, bod)
              | _ -> Either.Right (elm, bod))
            rows
        in
        let yesrow, yesbody = List.split yes in
        let norow, nobody = List.split no in
        let added_accessors =
          List.mapi
            (fun idx elm -> add_get_contr idx (List.hd accs))
            ctorbody
        in
        let yesmat : ('a, 'b) matrix =
          (added_accessors @ List.tl accs, yesrow, yesbody)
        in
        let nomat : ('a, 'b) matrix = (accs, norow, nobody) in
        if List.length yesbody = 0 && List.length nobody = 0 then
          failwith "body are empty ctor?"
        else
          let yes = compile yesmat in
          let no = compile nomat in
          let dummy = fresh_resolved () in
          Match
            ( data_bot (),
              List.hd accs,
              [ (CaseCtor (ctorname, []), yes); (CaseVar dummy, no) ]
            )
    | CaseLit lit ->
        let yes, no =
          partition_mapi
            (fun idx elm ->
              let bod = List.nth bodys idx in
              (* elm is a row *)
              match List.hd elm with
              | CaseLit l when l = lit ->
                  Either.Left (List.tl elm, bod)
              | _ -> Either.Right (elm, bod))
            rows
        in
        let yesrow, yesbody = List.split yes in
        let norow, nobody = List.split no in
        let yesmat : ('a, 'b) matrix =
          (List.tl accs, yesrow, yesbody)
        in
        let nomat : ('a, 'b) matrix = (accs, norow, nobody) in
        if List.length yesbody = 0 && List.length nobody = 0 then
          failwith "both are empty?"
        else begin
          print_endline "\ncompile yes:";
          let yes = compile yesmat in
          print_endline "\ncompile no:";
          let no = compile nomat in
          let dummy = fresh_resolved () in
          Match
            ( data_bot (),
              List.hd accs,
              [ (CaseLit lit, yes); (CaseVar dummy, no) ] )
        end

and compile_pre head (cases : ('a case * ('a, 'b) expr) list) : 'c =
  print_endline "compile pre:";
  List.iter
    (fun (a, b) ->
      print_endline "case:";
      print_endline (show_case pp_resolved a);
      print_endline "expr:;")
    cases;
  (* We assume that all the cases are of the same form -
     typechecking should guarantee this. 
     . *)
  if List.length cases = 0 then
    (* TODO: don't do this *)
    Fail (data_bot (), "Pattern match failure")
  else
    let rows, bodys = normalize_cases cases |> List.split in
    let fresh = fresh_resolved () in
    let matrix =
      ( [ Var (data_bot (), fresh) ],
        List.map (fun x -> [ x ]) rows,
        bodys )
    in
    print_endline "pre matrix:";
    print_endline (show_matrix' matrix);
    LetIn (data_bot (), CaseVar fresh, None, head, compile matrix)

let rec pattern_comp (expr : ('a, resolved typ) expr) : ('a, 'b) expr
    =
  let go = pattern_comp in
  match expr with
  | LetIn (data, case, ty, head, body) -> (
      match case with
      | CaseWild ->
          LetIn
            (data, CaseVar (fresh_resolved ()), ty, go head, go body)
      | CaseVar _ -> LetIn (data, case, ty, go head, go body)
      | _ ->
          (* Turn the let into a match, which can be properly handled *)
          go @@ Match (data, head, [ (case, body) ]))
  | Seq (data, l, r) -> Seq (data, go l, go r)
  | Funccall (data, f, x) -> Funccall (data, go f, go x)
  | Binop (data, op, l, r) -> Binop (data, op, go l, go r)
  | UnaryOp (data, op, x) -> UnaryOp (data, op, go x)
  | Lambda (data, nm, ty, body) -> Lambda (data, nm, ty, go body)
  | Tuple (data, ts) -> Tuple (data, List.map go ts)
  | Annot (_, _, _) -> failwith "Annotation after typechecking"
  | Match (data, head, cases) -> compile_pre head cases
  | Modify (data, nm, expr) -> Modify (data, nm, go expr)
  | Record (data, nm, cases) ->
      Record (data, nm, List.map (fun (a, b) -> (a, go b)) cases)
  | otherwise -> expr

let pattern_match_desugar (top : ('a, 'b) toplevel list) :
    ('a, 'b) toplevel list =
  List.map
    (function
      | Definition def ->
          let comp'd = pattern_comp (get def.body) in
          print_endline "\ncompiled to:";
          print_endline
            (show_expr pp_resolved (pp_typ pp_resolved) comp'd);
          Definition { def with body = Just comp'd }
      | x -> x)
    top
