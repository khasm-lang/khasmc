open Ast
open Kenv

let rec do_all f a =
  match a with
  | [] -> ()
  | x :: xs -> f x; do_all f xs

let typecheckAssignBlock ast ctx sco =
  match ast with
  | Many x :: xs ->  
  | AssignBlock -> typecheckAssignBlock ast ctx (sco + 1)


let typecheckProgram ast ctx sco =
  match ast with
  | [] -> ()
  | x :: xs ->
     begin
      match x with
      | AssignBlock (b) -> typecheckAssignBlock b ctx (sco + 1)
      | Assign (a) -> typecheckAssign a ctx (sco + 1)
      | Typesig (i,t) ->
         addVar ctx {id = i; typesig = t; scope = sco};
     end;
     typecheckProgram xs ctx sco


let typecheckAst ast =
  let ctx = {parent = None; vars = []} in
  typecheckProgram ast ctx 0
