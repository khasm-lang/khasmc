open Ast
open Typecheck_env
open BatPervasives

exception UniqErr of string

type uniq_env = {
    parent: uniq_env option;
    binds : (string * string) list;
  }

let new_uniq_env () = {parent=None; binds=[]}

let get_uniq () =
  (string_of_int (unique ())) ^ "_tvar"


let rec get_uniq_in s env =
  match List.filter (fun x -> fst x = s) env.binds with
  | [] ->
     begin
       match env.parent with
       | Some(x) -> get_uniq_in s x
       | None -> s
     end
   | [x] -> snd x
   | x :: _ -> raise (UniqErr ("Same typevar \""
                                ^ (fst x)
                                ^ "\" declared multiple"
                                ^ " times in one rank"))

let rec mkbinds sl =
  match sl with
  | [] -> []
  | x :: xs -> (x, get_uniq ()) :: mkbinds xs

let add_binds sl env =
    {parent = Some(env); binds = mkbinds sl;}

and get_binds newenv =
  List.map (fun x -> snd x) newenv.binds


let rec make_uniq_base t env =
  match t with
  | KTypeBasic(s) ->
     KTypeBasic(get_uniq_in s env)
  | KTypeApp(t, s) ->
     KTypeApp(make_uniq_ts t (Some(env)), get_uniq_in s env)

and make_uniq_ts ts env =
  let env =
    match env with
    | None -> new_uniq_env ()
    | Some(x) -> x
  in
  match ts with
  | TSBase(t) -> TSBase(make_uniq_base t env)
  | TSMap(t, k) -> TSMap(make_uniq_ts t (Some(env))
                       , make_uniq_ts k (Some(env)))
  | TSForall(sl, t) ->
     let newenv = add_binds sl env in
     TSForall(get_binds newenv, make_uniq_ts t (Some(newenv)))
  | TSTuple(x) ->
     TSTuple(List.map (fun x -> make_uniq_ts x (Some(env))) x)
  | TSAdHoc(x) ->
     TSAdHoc(List.map (fun x -> make_uniq_ts x (Some(env))) x)

let rec make_uniq_toplevel t =
  match t with
  | TopAssign(_) -> t
  | TopTDecl(id, ts) -> TopTDecl(id, make_uniq_ts ts None)

let rec make_uniq_typevars program =
  match program with
  | Program([]) -> program
  | Program(x)  -> Program(List.map make_uniq_toplevel x)
