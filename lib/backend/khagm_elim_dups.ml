open Exp
open Khagm

let rec subsval list oldid newid =
  let rec go elm =
    match elm with
    | LetInVal (ret, Val v) ->
        if v = oldid then
          LetInVal (ret, Val newid)
        else
          elm
    | LetInCall (ret, func, args) ->
        let func =
          if func = oldid then
            newid
          else
            func
        in
        LetInCall
          ( ret,
            func,
            List.map
              (fun e ->
                if e = oldid then
                  newid
                else
                  e)
              args )
    | Special (ret, Val v, spec) ->
        if v = oldid then
          Special (ret, Val newid, spec)
        else
          elm
    | IfElse (ret, cond, e1, e2) ->
        let cond =
          if cond = oldid then
            newid
          else
            cond
        in
        IfElse (ret, cond, subsval e1 oldid newid, subsval e2 oldid newid)
    | SubExpr (i, l) -> SubExpr (i, subsval l oldid newid)
    | Return i ->
        if i = oldid then
          Return newid
        else
          elm
    | _ -> elm
  in
  List.map go list

let rec elim_expr e =
  match e with
  | [] -> []
  | x :: xs -> (
      match x with
      | Ref _ | Unref _ | Return _
      | LetInCall (_, _, _)
      | Special (_, _, _)
      | CheckCtor (_, _, _)
      | Fail _ ->
          x :: elim_expr xs
      | IfElse (ret, cond, e1, e2) ->
          IfElse (ret, cond, elim_expr e1, elim_expr e2) :: elim_expr xs
      | SubExpr (i, l) -> SubExpr (i, elim_expr l) :: elim_expr xs
      | LetInVal (id, Val v) ->
          let xs' = subsval xs id v in
          elim_expr xs'
      | LetInVal (_, _) -> x :: elim_expr xs)

let elim_dups_h top =
  match top with
  | Ctor (_, _) | Extern (_, _, _) | Noop -> top
  | Let (name, args, body) -> Let (name, args, elim_expr body)

let elim_dups khagm =
  let code, map = khagm in
  let f = List.map elim_dups_h in
  (Opt.to_fixpoint 1000 f code, map)
