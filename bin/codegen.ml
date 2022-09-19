open Kenv
open Print_ast
open Typecheck
exception Impossible

let codegenProgram file prog =
  print_endline ("doing the thing for" ^ file);
  "hurr durr code " ^ file ^ "\n" 

let rec codegenProgramList files progs =
  match files with
  | f :: fs -> f :: fs
  | [] -> ["hjmm"]
