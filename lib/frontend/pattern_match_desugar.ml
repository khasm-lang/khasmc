open Parsing.Ast
open Typecheck
open Share.Maybe
open Share.Uuid

(* columns inner *)
type 'a actions = ('a, unit) expr list
type 'a matrix = 'a case list list
type 'a t = 'a matrix * 'a actions

let make_first (matrix : 'a matrix) i : 'a matrix =
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

let untuple (name : 'a) (data : _ data) (cases : 'a case list)
    (actions : 'a actions) : 'a t =
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

(*
  After this step all lets are guaranteed to be in the form
  let v = ... in ...
 *)

let compile_match data (head : ('a, 'b) expr)
    (cases : ('a case * ('a, 'b) expr) list) : ('a, 'b) expr =
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
  let cases, exprs = List.split cases in
  let fresh = fresh_resolved () in
  failwith "pattern match compilation"

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
      | Definition d ->
          Definition { d with body = Just (go (get d.body)) }
      | x -> x)
    top
