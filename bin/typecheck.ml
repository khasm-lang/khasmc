open Exp
open Typecheck_env
open Ast

let rec oop () = ()

and typecheck_base b f env =
  match b with
  | Ident(x) ->
     begin
       match find_either_in_env x env with
       | Some(l) -> TSPermute(List.map (fun x -> x.ts) l)
       | None -> raise (NotFound ("Not found:\n"
                                  ^ str_of_fident x))
     end
  | Int(_) -> typesig_of_str "int"
  | Float(_) -> typesig_of_str "float"
  | Str(_) -> typesig_of_str "string"
  | Tuple(t) -> TSTuple(List.map (fun x -> typecheck_expr x f env) t)
     
and typecheck_program p f env =
  match p with
  | Program([]) -> env
  | Program(x :: xs) ->
     let newenv =
       match x with
       | TopAssign((id, ts), assign) ->
          typecheck_assign assign f ts env;
          add_to_env id f ts env
     in
     typecheck_program (Program(xs)) f newenv

and typecheck_program_list progs files env =
  let env = match env with
    | None -> new_env ()
    | Some(x) -> x
  in
  match (progs, files) with
  | ([], []) -> env
  | (p :: ps, f :: fs) ->
     let tmp = typecheck_program p [f] env in 
     typecheck_program_list ps fs (Some(tmp))
  | (_, _) -> raise (Impossible "unbalanced prog/files")
