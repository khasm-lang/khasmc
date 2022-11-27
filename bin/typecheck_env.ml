open Ast
exception Impossible of string

let rec unqual_name x =
  match x with
  | Bot(y) -> y
  | Mod(_, y) -> unqual_name y
  | Struc(_, y) -> unqual_name y

let qual_name x = x

let pname_from_kident x = Bot(x)

type obj = {
    nm : fident;
    ts: typesig;
  }

type env = {
    parent: env option;
    objs: obj list;
  }

let new_env () =
  {parent = None; objs=[];}

let new_env_parent parent =
  {parent = parent; objs=[];}


let add_to_env unqual ql ts prev =
  {
    prev with objs =
               {nm = mod_from_list ql unqual; ts=ts}
               :: prev.objs
  }

let rec find_qual_in_env ql en =
  match List.filter (fun x -> qual_name x.nm = ql) en.objs with
  | x :: xs -> Some(x :: xs)
  | [] ->
     match en.parent with
     | Some(x) -> find_qual_in_env ql x
     | None -> None

let rec find_unqual_in_env ql en =
  match List.filter (fun x -> unqual_name x.nm = ql) en.objs with
  | x :: xs -> Some(x :: xs)
  | [] ->
     match en.parent with
     | Some(x) -> find_unqual_in_env ql x
     | None -> None


(*
  TODO:
  this currently does not support stuff like
  (∀a, (∀a, a -> a) -> a)
  where inner a and outer a should be different typevars.
  FIX:
  add a step before typechecking where all typevars are fixed to be
  their own thing, just using like, counting upwards in letters
 *)

exception TypeError of string

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

let show_list f lst =
  let rec print_elements = function
    | [] -> ""
    | h::t -> f h ^ ";" ^ print_elements t
  in
  "[" ^ print_elements lst ^ "]"

let show_strlist sl = show_list (fun x -> x) sl

let rec args_ts_to_objs args ts =
  match args with
  | [] -> []
  | ar :: ars ->
     try
       {nm = Bot(ar); ts = ts_get_left ts}
       :: args_ts_to_objs ars (ts_get_right ts)
     with Not_found ->
       raise (TypeError
                ("args dont match typesig:"
                 ^ show_strlist args
                 ^ "\n"
                 ^ show_typesig ts))

let add_args_to_env args ts env =
  {env with objs = args_ts_to_objs args ts @ env.objs}

let rec return_type_of args ts =
  match args with
  | [] -> ts
  | _ :: xs ->
     try
       return_type_of xs (ts_get_right ts)
     with Not_found ->
       raise (TypeError
                ("args dont match typesig:"
                 ^ show_strlist args
                 ^ "\n"
                 ^ show_typesig ts))
