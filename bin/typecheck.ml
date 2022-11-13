open Unify
open Ast
open Typecheck_env

exception TypecheckFailure of string 

let typecheck_failure err x y =
  print_endline "Failed to unify :\n";
  print_endline (show_typesig x);
  print_endline "\nand:\n";
  print_endline (show_typesig y);
  print_endline "\ndue to:\n";
  print_endline err;
  raise (TypecheckFailure err)


let rec typecheck_program p f env =
  match p with
  | Program(x :: xs) ->
     begin
       match x with
       | TopTDecl(k) ->
          let name = fst k in
          let ts = snd k in
          let qname = qual_name name f in
          let tenv = add_func_to_env qname env in
          let newenv = add_tsig_to_func qname ts tenv in
          typecheck_program (Program(xs)) f newenv
       | TopAssign(x) ->
          begin
            (*
              TODO:
              we need to loop here, checking all possible type sigs
              this is due to ad hoc polymorphism
             *)
            match x with
            | KAss(name, args, expr) ->
               ()
          end
     end
  | _ -> raise (Impossible "typecheck_program")
