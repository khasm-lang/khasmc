open Exp
open Khagm

(** Turn calls with sufficient args into direct calls *)

let rec unbox_calls arties code =
  match code with
  | LetInCall (ret, func, args) -> (
      match List.assoc_opt func arties with
      | None -> code
      | Some n ->
          if n = List.length args then
            LetInUnboxCall (ret, func, args)
          else
            code)
  | IfElse (ret, i, e1, e2) ->
      let e1' = List.map (unbox_calls arties) e1 in
      let e2' = List.map (unbox_calls arties) e2 in
      IfElse (ret, i, e1', e2')
  | SubExpr (i, e) ->
      let e' = List.map (unbox_calls arties) e in
      SubExpr (i, e')
  | _ -> code

let rec unbox_top arties code =
  match code with
  | Let (id, args, body) -> Let (id, args, List.map (unbox_calls arties) body)
  | _ -> code

let rec get_arties code =
  match code with
  | [] -> []
  | Let (nm, args, _body) :: xs -> (nm, List.length args) :: get_arties xs
  | Extern (nm, arity, _realnm) :: xs -> (nm, arity) :: get_arties xs
  | Ctor (nm, arity) :: xs -> (nm, arity) :: get_arties xs
  | _ :: xs -> get_arties xs

let rec unbox_calls (code, tbl) =
  let arties = get_arties code in
  (List.map (unbox_top arties) code, tbl)
