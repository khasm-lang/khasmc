open Ast
open Exp
open Typecheck

let init_toplevel t =
  match t with
  | TopAssign ((_, ts), (id, args, expr)) ->
      TopAssign
        ((id, ts), (id, args, conv_ts_args_body_to_typelams ts args expr))
  | Extern (_, _) -> t
  | IntExtern (_, _, _) -> t

let init_program p =
  Program (List.map init_toplevel (match p with Program x -> x))
