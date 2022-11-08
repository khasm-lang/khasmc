open Ast

exception TypeError of string

let rec typecheckToplevelList tl =
  match tl with
  | [] -> ()
  | x :: xs ->
     

let typecheckProgram p =
  match p with
  | Program(x) -> typecheckToplevelList x
