open Ast
open Kenv

let rec do_all f a =
  match a with
  | [] -> ()
  | x :: xs -> f x; do_all f xs

let varNotFound ctx i =
  print_endline ("Variable " ^ i ^ " not found")

let rec typecheckToplevel ast ctx sco =
  match ast with
  | Typesig (i, t) ->
     let var = {id = i; typesig = t; scope=sco} in
     addVar ctx var
  | AssignBlock (i, b) ->
     begin
       try
         let var = findVar ctx i in
         let possible = typecheckBlockReturns b ctx sco in
         List.iter (String.equals var.typesig)  
       with Not_found -> varNotFound ctx i 
     end
  | Assign (i, e) -> ()
let rec typecheckProgram ast ctx sco =
  match ast with
  | [] -> ()
  | (x :: xs) ->
     let newctx = typecheckToplevel x ctx sco in
     typecheckProgram xs newctx sco

let typecheckAst ast =
  let ctx = {parent = None; vars = []} in
  typecheckProgram ast ctx 0
