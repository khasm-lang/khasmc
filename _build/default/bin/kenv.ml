open Ast

(* Represents a single type *)
type var_t = {
    id : string;
    ts : typeSig;
    at : attr list;
  }

open Print_ast

let print_var_t t =
  Printf.printf "%s " t.id;
  printTypesig (t.ts) 0;
  Printf.printf "\n"

let print_var_t_list l = List.iter print_var_t l


(* represents a single context - if & while blocks
 inherit ts, vars & args from their parent *)
type context = {
    parent : context option;
    inherit_ctx : bool;
    ts : typeSig option;
    vars : var_t list;
    args : var_t list;
  }

let addVar ctx var =
  {ctx with
    vars = var :: ctx.vars}

let rec findVarCtx varlist id =
  match varlist with
  | [] -> raise Not_found
  | x :: xs ->
     if x.id = id then
       x
     else
       findVarCtx xs id

let rec findVar ctx id =
  try
    Some(findVarCtx ctx.vars id)
  with Not_found ->
        try
          Some(findVarCtx ctx.args id)
        with Not_found ->
              match ctx.parent with
              | None -> None
              | Some (x) -> findVar x id
