open Exp
open Khagm

(** Turns calls to functions that immediatly return into direct *)

let rec sub subs i = match List.assoc_opt i subs with None -> i | Some n -> n

let rec remove_val subs v =
  match v with
  | Val i -> Val (sub subs i)
  | Tuple l -> Tuple (List.map (remove_val subs) l)
  | _ -> v

let rec remove_expr subs code =
  match code with
  | Fail s -> code
  | LetInVal (i, v) -> LetInVal (i, remove_val subs v)
  | LetInCall (i, func, args) ->
      LetInCall (i, sub subs func, List.map (sub subs) args)
  | LetInUnboxCall (i, func, args) ->
      LetInUnboxCall (i, sub subs func, List.map (sub subs) args)
  | IfElse (i, cond, e1, e2) ->
      IfElse
        ( i,
          sub subs cond,
          List.map (remove_expr subs) e1,
          List.map (remove_expr subs) e2 )
  | Special (i, v, spec) -> Special (i, remove_val subs v, spec)
  | SubExpr (i, e) -> SubExpr (i, List.map (remove_expr subs) e)
  | CheckCtor (i, arr, int) -> code
  | Return i -> Return (sub subs i)
  | Unref i -> Unref (sub subs i)
  | Ref i -> Ref (sub subs i)

let rec remove_top subs code =
  match code with
  | [] -> []
  | x :: xs -> (
      match x with
      | Let (id, [], [ Return i ]) -> x :: remove_top ((id, i) :: subs) xs
      | Let (id, args, code) ->
          Let (id, args, List.map (remove_expr subs) code) :: remove_top subs xs
      | _ -> x :: remove_top subs xs)

let remove_zeroarity (code, tbl) = (remove_top [] code, tbl)
