open Ast
open Subtype
open SubtypeBinOp
open Typecheck_env

let rec bot = ()

and typecheck_assign tup fname env =
  match tup with
  | KAss(id, arg, expr) ->
     ()

and typecheck_program prog fname env =
  let env =
    match env with
    | Some(x) -> x
    | None -> new_env ()
  in 
  match prog with
  | x :: xs ->
     begin
       let newenv =
         match x with
         | TopTDecl (id, ts) ->
            add_to_env id fname ts env
         | TopAssign(x) ->
            typecheck_assign x fname env;
            env
       in
       typecheck_program xs fname (Some(newenv))
     end
  | [] -> ()
