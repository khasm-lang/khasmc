open Parsing.Ast
open Typecheck
open Share.Maybe
open Share.Uuid

(* columns inner *)
type actions = (resolved, unit) expr list
[@@deriving show { with_path = false }]

(* we need to store which row this was originally
   in order to be able to access it correctly
 *)
type matrix = (resolved case list * int) list
[@@deriving show { with_path = false }]

let make_first (matrix : matrix) i : matrix =
  let rec go xs k =
    match (xs, k) with
    | [], _ -> failwith "make_first invalid"
    | x :: xs, 0 -> (x, xs)
    | x :: xs, i ->
        let first, rest = go xs (i - 1) in
        (first, x :: rest)
  in
  let first, rest = go matrix i in
  first :: rest

let annotate_position x = List.mapi (fun i a -> (a, i)) x

let rec transpose = function
  | [] -> []
  | [] :: _ -> []
  | rest -> List.map List.hd rest :: transpose (List.map List.tl rest)

let untuple (name : 'a) (data : _ data) (cases : 'a case list)
    (actions : actions) : matrix * actions =
  List.map2
    (fun case action ->
      match case with
      | CaseWild -> ([ CaseWild ], action)
      | CaseVar a ->
          let data_let =
            data_of @@ add_type_with_existing data.uuid
          in
          let data_var =
            data_of @@ add_type_with_existing data.uuid
          in
          ( [ CaseWild ],
            LetIn
              (data_let, CaseVar a, None, Var (data_var, name), action)
          )
      | CaseTuple t -> (t, action)
      | CaseCtor (a, ls) -> ([ CaseCtor (a, ls) ], action)
      | CaseLit l -> ([ CaseLit l ], action))
    cases actions
  |> List.split
  |> fun (matrix, actions) ->
  let matrix' = transpose matrix in
  (annotate_position matrix', actions)

let only_in arr idxs = List.filteri (fun ix p -> List.mem ix idxs) arr

let only_not_in arr idxs =
  List.filteri (fun ix p -> not @@ List.mem ix idxs) arr

let split_by_in arr idxs =
  let rec go k = function
    | [] -> ([], [])
    | x :: xs ->
        let yes, no = go (k + 1) xs in
        if List.mem k idxs then
          (x :: yes, no)
        else
          (yes, x :: no)
  in
  go 0 arr

let irrefutable (matrix : matrix) : bool =
  if List.length matrix = 0 then
    failwith "irrefutable empty";
  matrix
  |> List.map (fun x -> List.nth (fst x) 0)
  |> List.for_all (function
       | CaseWild -> true
       | CaseVar _ -> true
       | _ -> false)

let find_refutable (matrix : matrix) : matrix =
  if irrefutable matrix then
    failwith "find refutable on irrefutable";
  if List.length matrix = 0 then
    failwith "find refutable on len 0";
  match
    List.find_mapi
      (fun i x ->
        match x with
        | CaseWild -> None
        | CaseVar _ -> None
        | _ -> Some i)
      (fst @@ List.nth matrix 0)
  with
  | None -> failwith "refutable irrefutable confusion"
  | Some k -> make_first matrix k

let rec compile_matrix (data : 'b data) (nm : 'a) (matrix : matrix)
    (actions : actions) : ('a, 'b) expr =
  print_endline ("matrix: " ^ show_matrix matrix);
  if List.length matrix = 0 then
    failwith "pattern match compilation";
  if irrefutable matrix then
    List.nth actions 0
  else if List.length matrix = 1 then begin
    let matrix, actions =
      untuple nm data (fst @@ List.nth matrix 0) actions
    in
    print_endline ("matrix after: " ^ show_matrix matrix);
    if List.length matrix = 1 then
      (* no tuples left; we're at a base level *)
      assemble_match data nm matrix actions
    else
      compile_matrix data nm matrix actions
  end
  else
    let matrix' = find_refutable matrix in
    let ret = assemble_match data nm matrix' actions in
    failwith "nontriv"

and assemble_match (data : 'b data) (nm : 'a) (matrix : matrix)
    (actions : actions) : ('a, 'b) expr =
  failwith "assemble match"

let compile_match data (head : ('a, 'b) expr)
    (cases : ('a case * ('a, 'b) expr) list) : ('a, 'b) expr =
  print_endline "compile match:";
  print_endline
    (show_expr pp_resolved
       (fun a b -> ())
       (Match (data, head, cases)));
  (*
    aim to turn
    match expr with
    ...
    end

    into
    let fresh = expr in
    match fresh with
    ...
    end
   *)
  let cases, actions = List.split cases in
  let fresh = fresh_resolved () in
  let comp = compile_matrix data fresh [ (cases, 0) ] actions in
  failwith "todo"

(*
  After this step all lets are guaranteed to be in the form
  let v = ... in ...
 *)

let compile_let data (case : 'a case) (head : ('a, 'b) expr)
    (body : ('a, 'b) expr) : ('a, 'b) expr =
  match case with
  | CaseVar c -> LetIn (data, case, None, head, body)
  | _ -> compile_match data head [ (case, body) ]

let pattern_match_desugar (top : ('a, 'b) toplevel list) :
    ('a, 'b) toplevel list =
  let rec go = function
    | LetIn (data, case, _, head, body) ->
        go (compile_let data case head body)
    | Match (data, head, cases) -> go (compile_match data head cases)
    | Seq (d, a, b) -> Seq (d, go a, go b)
    | Funccall (d, a, b) -> Funccall (d, go a, go b)
    | Binop (d, op, a, b) -> Binop (d, op, a, b)
    | UnaryOp (d, op, v) -> UnaryOp (d, op, go v)
    | Lambda (d, nm, ty, bd) -> Lambda (d, nm, ty, go bd)
    | Tuple (d, t) -> Tuple (d, List.map go t)
    | Annot _ -> failwith "annot after monomorphization"
    | Record (d, nm, cases) ->
        Record (d, nm, List.map (fun (a, b) -> (a, go b)) cases)
    | x -> x
  in
  List.map
    (function
      | Definition d -> begin
          print_endline ("pattern compilin: " ^ show_resolved d.name);
          Definition { d with body = Just (go (get d.body)) }
        end
      | x -> x)
    top
