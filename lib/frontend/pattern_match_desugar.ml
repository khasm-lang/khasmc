open Parsing.Ast
open Typecheck
open Share.Maybe
open Share.Uuid

(* columns inner *)
type actions = (resolved, unit) expr list
[@@deriving show { with_path = false }]

(* store it alongside an expression which can be used to
   access that row
 *)
type matrix = (resolved case list * ((resolved, unit) expr[@opaque])) list
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

let untuple (accessor_data : _ data) (cases : 'a case list)
      (accessor : ('a, 'b) expr)
      (actions : actions) : matrix * actions =
  List.map2
    (fun case action ->
      match case with
      | CaseWild -> ([ CaseWild ], action)
      | CaseVar a ->
          ( [ CaseWild ],
            LetIn
              (data (), CaseVar a, None, accessor, action)
          )
      | CaseTuple t -> (t, action)
      | CaseCtor (a, ls) -> ([ CaseCtor (a, ls) ], action)
      | CaseLit l -> ([ CaseLit l ], action))
    cases actions
  |> List.split
  |> fun (mat, actions) ->
     let matrix = transpose mat in
     if List.length matrix = 1 then
       [(List.nth matrix 0, accessor)], actions
     else
       List.mapi (fun idx elm ->
           elm, UnaryOp(data (), Project idx, accessor)
         ) matrix, actions
     
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

let rec group_by_constrs
          (first : 'a case list * ('a, 'b) expr)
          (rest : matrix)
          (actions: actions) : ('a case * matrix * actions) list
  =
  
  failwith "group by constrs"


let rec compile_matrix (data' : 'b data) (nm : 'a) (matrix : matrix)
    (actions : actions) : ('a, 'b) expr =
  print_endline ("matrix: " ^ show_matrix matrix);
  if List.length matrix = 0 then begin
      if List.length actions = 1 then
        List.hd actions
      else 
        failwith "pattern match compilation"
    end
  else if irrefutable matrix then begin
      print_endline "irrefutable?";
      assemble_match data' nm matrix actions
    end else if List.length matrix = 1 then begin
      let matrix, actions =
        untuple data'
          (fst @@ List.nth matrix 0)
          (Var(data',nm))
          actions
      in
      print_endline ("matrix after: " ^ show_matrix matrix);
      if List.length matrix = 1 then
        (* no tuples left; we're at a base level *)
        assemble_match data' nm matrix actions
      else
        compile_matrix data' nm matrix actions
    end
  else
    let matrix' = find_refutable matrix in
    let ret = assemble_match data' nm matrix' actions in
    failwith "nontriv"

and assemble_match (data : 'b data) (nm : 'a) (matrix : matrix)
(actions : actions) : ('a, 'b) expr =
  match matrix with
  | [] -> failwith "assemble empty matrix"
  | first :: rest ->
     let groups = group_by_constrs first rest actions in
     failwith "assemble_match"

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
  let comp = compile_matrix data fresh [ (cases, head) ] actions in
  print_endline "compiled match:";
  print_endline (show_expr pp_resolved (fun _ _ -> ()) comp);
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
