open Parsing.Ast
open Typecheck
open Share.Maybe
open Share.Uuid

(*
  After this step all lets are guaranteed to be in the form
  let v = ... in ...
 *)

let compile_match (head : ('a, 'b) expr)
    (cases : ('a case * ('a, 'b) expr) list) : ('a, 'b) expr =
  failwith "pattern match compilation"

let compile_let (case : 'a case) (head : ('a, 'b) expr)
    (body : ('a, 'b) expr) : ('a, 'b) expr =
  failwith "pattern match compile let"

let pattern_match_desugar (top : ('a, 'b) toplevel list) :
    ('a, 'b) toplevel list =
  let rec go = function
    | LetIn (_, case, _, head, body) ->
        go (compile_let case head body)
    | Match (_, head, cases) -> go (compile_match head cases)
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
