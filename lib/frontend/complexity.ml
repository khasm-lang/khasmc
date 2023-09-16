open Ast
open Exp

(* Computes a complexity for each node. Not currently used. *)

let cmpset inf complex = { inf with complex = complex + 1 }

let cmpget expr =
  match expr with
  | Base (inf, _)
  | FCall (inf, _, _)
  | LetIn (inf, _, _, _)
  | LetRecIn (inf, _, _, _, _)
  | IfElse (inf, _, _, _)
  | Join (inf, _, _)
  | Inst (inf, _, _)
  | Lam (inf, _, _)
  | TypeLam (inf, _, _)
  | TupAccess (inf, _, _)
  | AnnotLet (inf, _, _, _, _)
  | AnnotLam (inf, _, _, _)
  | Match (inf, _, _)
  | ModAccess (inf, _, _) ->
      inf.complex

let init_base b =
  match b with Ident (inf, id) -> (Ident (cmpset inf 0, id), 1) | _ -> (b, 1)

let rec init_expr expr =
  match expr with
  | Base (inf, base) ->
      let bd, cmplx = init_base base in
      Base (cmpset inf cmplx, bd)
  | FCall (inf, e1, e2) ->
      let b1 = init_expr e1 in
      let b2 = init_expr e2 in
      FCall (cmpset inf (cmpget b1 + cmpget b2), b1, b2)
  | LetIn (inf, id, e1, e2) ->
      let b1 = init_expr e1 in
      let b2 = init_expr e2 in
      LetIn (cmpset inf (cmpget b1 + cmpget b2), id, b1, b2)
  | LetRecIn (inf, ts, id, e1, e2) ->
      let b1 = init_expr e1 in
      let b2 = init_expr e2 in
      LetRecIn (cmpset inf (cmpget b1 + cmpget b2), ts, id, b1, b2)
  | IfElse (inf, c, e1, e2) ->
      let b1 = init_expr c in
      let b2 = init_expr e1 in
      let b3 = init_expr e2 in
      IfElse (cmpset inf (cmpget b1 + cmpget b2 + cmpget b3), b1, b2, b3)
  | Join (inf, e1, e2) ->
      let b1 = init_expr e1 in
      let b2 = init_expr e2 in
      Join (cmpset inf (cmpget b1 + cmpget b2), b1, b2)
  | Inst (_, _, _) -> raise (Impossible "inst init_expr complexity")
  | Lam (inf, id, bd) ->
      let bd' = init_expr bd in
      Lam (cmpset inf (cmpget bd'), id, bd')
  | TypeLam (inf, id, bd) ->
      let bd' = init_expr bd in
      TypeLam (cmpset inf (cmpget bd'), id, bd')
  | TupAccess (inf, bd, int) ->
      let bd' = init_expr bd in
      TupAccess (cmpset inf (cmpget bd'), bd', int)
  | AnnotLet (inf, id, ts, e1, e2) ->
      let b1 = init_expr e1 in
      let b2 = init_expr e2 in
      AnnotLet (cmpset inf (cmpget b1 + cmpget b2), id, ts, b1, b2)
  | AnnotLam (inf, id, ts, e) ->
      let b = init_expr e in
      AnnotLam (cmpset inf (cmpget b), id, ts, b)
  | ModAccess (inf, exp, id) ->
      ModAccess (cmpset inf (List.length exp + 1), exp, id)
  | Match (inf, p, pats) -> Match (inf, p, pats)

let rec init_toplevel t =
  match t with
  | Extern (_, _, _) -> t
  | IntExtern (_, _, _, _) -> t
  | Bind (_, _, _) -> t
  | Open _ -> t
  | Typealias (_, _, _) -> t
  | Typedecl (_, _, _) -> t
  | TopAssignRec (a, (id, args, body)) ->
      let body' = init_expr body in
      TopAssignRec (a, (id, args, body'))
  | TopAssign (a, (id, args, body)) ->
      let body' = init_expr body in
      TopAssign (a, (id, args, body'))
  | SimplModule (a, bd) -> SimplModule (a, List.map init_toplevel bd)

let init_program p =
  match p with Program x -> Program (List.map init_toplevel x)
