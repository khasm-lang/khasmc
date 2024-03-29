open Ast
(* Wraps files into modules *)

let wrap_in name program =
  match program with
  | Program p -> Program [ SimplModule (dummyinfo2, name, p) ]
