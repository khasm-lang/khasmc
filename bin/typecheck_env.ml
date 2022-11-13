open Ast
exception Impossible of string

type func = {
    qual_name   : string;
    tsigs : typesig list;
  }

type env = {
    prefix : string;
    parent : env option;
    binds : func list;
  }

let qual_name name fname = fname ^ "." ^ name

let new_env pre = {parent = None; binds = []; prefix = pre}

let new_func prefix name =
  {qual_name = prefix ^ name; tsigs = []}


let rec find_func_in_env name env =
  match List.filter (fun x -> x.qual_name = name) env.binds with
  | x :: xs ->
     if xs <> [] then
       raise (Impossible "Multiple funcs returned find_func_in_env")
     else
       Some(x)
  | [] ->
     match env.parent with
     | Some(y) -> find_func_in_env name y
     | None -> None

let add_func_to_env name env =
  match find_func_in_env name env with
  | Some(_) -> env
  | None -> {env with binds = {qual_name = name; tsigs = []} :: env.binds}

let add_tsig_to_func name tsig env =
  match find_func_in_env name env with
  | Some(ne) ->
     begin
       let without = List.filter (fun x -> x.qual_name <> name) env.binds in
       let new_func = {ne with tsigs = tsig :: ne.tsigs} in
       {env with binds = new_func :: without}
     end
  | None -> raise Not_found

(*
  TODO:
  this currently does not support stuff like
  (∀a, (∀a, a -> a) -> a)
  where inner a and outer a should be different typevars.
  FIX:
  add a step before typechecking where all typevars are fixed to be
  their own thing, just using like, counting upwards in letters
 *)

let rec ts_get_left ts =
  match ts with
  | TSBase(_) -> raise Not_found
  | TSMap(a, _) -> a
  | TSForall(sl, t) ->
     begin
       match ts_get_left t with
       | TSForall(nl, a) -> TSForall(nl @ sl, a)
       | x -> TSForall(sl, x)
     end
  | TSTuple(_) -> raise Not_found

let rec ts_get_right ts =
  match ts with
  | TSBase(_) -> raise Not_found
  | TSMap(_, a) -> a
  | TSForall(sl, t) ->
     begin
       match ts_get_right t with
       | TSForall(nl, a) -> TSForall(nl @ sl, a)
       | x -> TSForall(sl, x)
     end
  | TSTuple(_) -> raise Not_found


let add_assign_to_env ts args env =
  env
