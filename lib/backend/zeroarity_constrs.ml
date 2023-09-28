open Exp
open Khagm

(** Turn assigns to zeroarity constructors into calls *)

let rec unbox_calls arties code =
  List.map
    (function
      | LetInVal (id, Val i) ->
          if List.mem i arties then
            LetInCall (id, i, [])
          else
            LetInVal (id, Val i)
      | IfElse (ret, cond, e1, e2) ->
          let e1' = unbox_calls arties e1 in
          let e2' = unbox_calls arties e2 in
          IfElse (ret, cond, e1', e2')
      | SubExpr (ret, e) ->
          let e' = unbox_calls arties e in
          SubExpr (ret, e')
      | x -> x)
    code

let rec unbox_top arties code =
  match code with
  | Let (id, args, body) -> Let (id, args, unbox_calls arties body)
  | _ -> code

let rec get_arties code =
  match code with
  | [] -> []
  | Ctor (nm, 0) :: xs -> nm :: get_arties xs
  | _ :: xs -> get_arties xs

let rec zeroarity_constrs (code, tbl) =
  let arties = get_arties code in
  (List.map (unbox_top arties) code, tbl)
