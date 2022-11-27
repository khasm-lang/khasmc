open Ast
open Subtype
open SubtypeBinOp
open Typecheck_env
exception NotFound of string
exception NotImpl of string

let rec bot () = print_endline "oop! this function isn't meant to be called"

and typeof_kbase bs fname env =
  match bs with
  | Int(_) -> typesig_of_str "Int"
  | Float(_) -> typesig_of_str "Float"
  | Str(_) -> typesig_of_str "Str"
  | _ -> raise (NotImpl "typeof_kbase")

and typecheck_expr expr fname env =
  match expr with
  | Base(x) -> typeof_kbase x fname env
  | Paren(x) -> typecheck_expr x fname env
  | Join(x, y) -> ignore (typecheck_expr x fname env); typecheck_expr y fname env
  | _ -> raise (NotImpl "typecheck_expr")
    
and typecheck_assign_h typs fname arg expr env =
  match typs with
  | [] -> ()
  | t :: ts ->
     begin
       let newenv = add_args_to_env arg t.ts in
       let return = typecheck_expr expr fname newenv in
       raise (NotImpl "typecheck_assign_h")
     end;
     typecheck_assign_h ts fname arg expr env

and typecheck_assign tup fname env =
  match tup with
  | KAss(id, arg, expr) ->
     match find_unqual_in_env id env with
     | Some(typs) -> typecheck_assign_h typs fname arg expr env
     | None -> raise (NotFound ("can't find " ^ id ^ " in env"))

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
