open Ast
open Exp
open Typecheck

(* Converts assignments into lambdas, allowing for typechecking *)

let rec init_toplevel t = t
let init_program p = p
