open Ast

let uniq = ref 0
let muniq = ref 0

let unique () =
  let x = !uniq in
  uniq := !uniq + 1;
  x

let munique () =
  let x = !muniq in
  muniq := !muniq + 1;
  x

let reset_uniq () = uniq := 0
let get_uniq () = string_of_int (unique ()) ^ "_t"
let get_meta () = "$m" ^ string_of_int (munique ())
let get_uniq_with s = string_of_int (unique ()) ^ s

exception UniqErr of string

type uniq_env = {
  parent : uniq_env option;
  binds : (string * string) list;
  typlams : string list;
}

let new_uniq_env () = { parent = None; binds = []; typlams = [] }

let rec get_uniq_in s env =
  match List.filter (fun x -> fst x = s) env.binds with
  | [] -> ( match env.parent with Some x -> get_uniq_in s x | None -> s)
  | [ x ] -> snd x
  | x :: _ ->
      raise
        (UniqErr
           ("Same typevar \""
           ^ fst x
           ^ "\" declared multiple"
           ^ " times in one rank"))

let rec mkbind s = (s, get_uniq ())

and add_binds s env =
  { env with parent = Some env; binds = mkbind s :: env.binds }

and get_binds newenv = List.nth newenv.binds 0 |> snd

and make_uniq_ts ts env =
  let env = match env with None -> new_uniq_env () | Some x -> x in
  match ts with
  | TSBase t -> TSBase (get_uniq_in t env)
  | TSMap (t, k) -> TSMap (make_uniq_ts t (Some env), make_uniq_ts k (Some env))
  | TSForall (s, t) ->
      let newenv = add_binds s env in
      TSForall (get_binds newenv, make_uniq_ts t (Some newenv))
  | TSTuple x -> TSTuple (List.map (fun x -> make_uniq_ts x (Some env)) x)
  | TSApp (x, y) -> TSApp (List.map (fun x -> make_uniq_ts x (Some env)) x, y)
  | TSMeta m -> TSMeta m

let rec make_uniq_toplevel t =
  match t with
  | Extern (_, _, _, _) -> t
  | TopAssign (info, id, ts, args, body) ->
      TopAssign (info, id, make_uniq_ts ts None, args, body)
  | _ -> t

let rec make_uniq_typevars program =
  match program with
  | Program [] -> program
  | Program x -> Program (List.map make_uniq_toplevel x)
