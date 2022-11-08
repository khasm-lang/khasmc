open Ast

type entry = {
    ts: typesig;
    id: kident;
  } [@@deriving make]

type typecheck_env = {
    parent: typecheck_env option;
    entries: entry list;
  } [@@deriving make]

let rec find_in_entry_list id l =
  match l with
  | [] -> raise Not_found
  | x :: xs ->
     if x.id == id then
       x
     else
       find_in_entry_list id xs

let rec find_in_ts_env id en =
  try
    find_in_entry_list id en.entries
  with Not_found ->
    match en.parent with
    | Some(x) -> find_in_ts_env id x
    | None -> raise Not_found

type typevar = string

type typesig_alias = typevar * (typevar, typesig) Either.t

type typesig_env = {
    parent: typesig_env option;
    alias_list: typesig_alias list;
    (* list of forall aliases in monomorphism process *)
    forall: kident list;
    (* tracks forall x y, ... *)
  } [@@deriving make]

exception Taken

let rec typesig_env_already_aliased ts_env id =
  match List.exists (fun x -> (fst x) == id) ts_env.alias_list with
  | true -> true
  | false ->
     match ts_env.parent with
     | Some(x) -> typesig_env_already_aliased x id
     | None -> false

let rec typesig_env_is_forall ts_env id =
  match List.exists (fun x -> x == id) ts_env.forall with
  | true ->
     begin
     match typesig_env_already_aliased ts_env id with
     | true -> raise Taken
     | false -> true
     end
  | false ->
     match ts_env.parent with
     | Some(x) -> typesig_env_is_forall x id
     | None -> false

let rec unify_typesig en ts1 ts2 =
  match ts1 with
  | 
