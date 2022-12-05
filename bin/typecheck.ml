open Ast
open Subtype
open SubtypeBinOp
open Typecheck_env
exception NotFound of string
exception NotImpl of string
exception TypeErr of string
let rec oop () = print_endline
                   "oop! this function isn't meant to be called"

and typeof_kbase bs fname env =
  match bs with
  | Int(_) -> [typesig_of_str "Int"]
  | Float(_) -> [typesig_of_str "Float"]
  | Str(_) -> [typesig_of_str "Str"]

  | Tuple(l) ->
     [
       TSTuple
        (List.map (fun x -> typecheck_expr x fname env) l)
     ]

  | Ident(i) ->
     match find_either_in_env i env with
     | Some(x) -> List.map (fun x -> x.ts) x
     | None ->
        raise (NotFound ("Can't find " ^ (str_of_fident i) ^ " in env"))

and typecheck_expr expr fname env =
  match expr with
  | Base(x) -> TSAdHoc(typeof_kbase x fname env)
  | Paren(x) -> typecheck_expr x fname env
  | Join(x, y) ->
     ignore (typecheck_expr x fname env); typecheck_expr y fname env
  | _ -> raise (NotImpl "typecheck_expr")
    
and typecheck_assign_h typs fname arg expr env =
  match typs with
  | [] -> ()
  | t :: ts ->
     let newenv = add_args_to_env arg t.ts env in
     let eret = typecheck_expr expr fname newenv in
     let aret = return_type_of arg t.ts in
     match eret with
     | TSAdHoc(y) ->
        begin
          match List.filter (fun x  -> x = aret) y with
          | [] -> raise
                    (TypeErr ("expected type :\n"
                              ^ str_of_typesig aret
                              ^ "\n doesn't match given:\n"
                              ^ str_of_typesig eret))
          | _ -> ()
        end
     | _ ->
        if eret = aret then
          ()
        else
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
  | Program(x :: xs) ->
     begin
       let newenv =
         match x with
         | TopTDecl (id, ts) ->
            add_to_env id fname ts env
         | TopAssign(x) ->
            typecheck_assign x fname env;
            env
       in
       typecheck_program (Program(xs)) fname (Some(newenv))
     end
  | Program([]) -> env

let rec typecheck_program_list progs (fnames : string list) env =
  match (progs, fnames) with
  | ([p], [f]) -> typecheck_program p [f] env
  | (p :: ps, f :: fs) ->
     Some(typecheck_program p [f] env) |> typecheck_program_list ps fs
  | (_, _) -> raise (Impossible "unbalanced files/progs")
