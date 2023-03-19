open Ast
open Exp
open Typecheck

let rec init_toplevel t =
  match t with
  | TopAssign ((_, ts), (id, args, expr)) ->
      TopAssign
        ((id, ts), (id, args, conv_ts_args_body_to_typelams ts args expr))
  | TopAssignRec ((_, ts), (id, args, expr)) ->
      TopAssignRec
        ((id, ts), (id, args, conv_ts_args_body_to_typelams ts args expr))
  | Extern (_, _, _) -> t
  | IntExtern (_, _, _, _) -> t
  | Bind (_, _, _) -> t
  | SimplModule (id, bd) -> SimplModule (id, List.map init_toplevel bd)

let init_program p =
  Program (List.map init_toplevel (match p with Program x -> x))
