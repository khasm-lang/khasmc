open Typecheck_env
open Ast

exception TypecheckFailure of string 

let typecheck_failure err x y =
  print_endline "Failed to unify :\n";
  print_endline (show_typesig x);
  print_endline "\nand:\n";
  print_endline (show_typesig y);
  print_endline "\ndue to:\n";
  print_endline err;
  raise (TypecheckFailure err)
