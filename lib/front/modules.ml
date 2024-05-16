open Ast
open Common.Info
open Common.Error
open Common

type ctx = {
  (* short, full *)
  definitions : (path * path) list;
  included : path list;
  open' : path list;
  current : string;
}
[@@deriving show { with_path = false }]

(* returns only the full matches as their longest path *)
let full_matches (paths : (path * path) list) (path : path) : path list =
  let rec go p1 p2 =
    match (p1, p2) with
    | End, End -> true
    | Base a, Base b when a = b -> true
    | InMod (a, q), InMod (b, w) ->
        if a = b then
          go q w
        else
          false
    | _ -> false
  in
  List.filter (fun pth -> go (fst pth) path) paths |> List.map snd

let rec similar_up_to path1 path2 =
  match (path1, path2) with
  | End, rest -> Some rest
  | InMod (a, q), InMod (b, w) when a = b -> similar_up_to q w
  | _ -> None

let rec find_naive_definition (ctx : ctx) (path : path) : (path, 'a) result =
  match path with
  | End -> err (`Invalid_Argument "Got End where full path was expected")
  | Base n -> (
      match full_matches ctx.definitions (InMod (ctx.current, Base n)) with
      | [] -> err (`No_Such_Variable path)
      | [ x ] -> ok x
      | _ -> err (`Overlapping_Variable path))
  | InMod (_mod, _tm) -> (
      match full_matches ctx.definitions path with
      | [] -> err (`No_Such_Variable path)
      | [ x ] -> ok x
      | _ -> err (`Overlapping_Variable path))

let handle_opens (ctx : ctx) : ctx =
  let remove_one path =
    List.map (fun (s, long) ->
        match similar_up_to path long with
        | Some p -> (p, long)
        | None -> (s, long))
  in
  let new_defs =
    List.fold_left (fun acc nm -> remove_one nm acc) ctx.definitions ctx.open'
  in
  { ctx with definitions = new_defs; open' = [] }

let ensure_includes_correct ctx path =
  match path with
  | End -> err (`Invalid_Argument "expected actual path, not end")
  | Base _ -> ok path
  | _ -> (
      List.map (fun x -> similar_up_to x path) ctx.included
      |> List.filter (fun x -> match x with Some _ -> true | None -> false)
      |> function
      | [] ->
          err (`Bad_Include ("No such path wrt includes: " ^ show_path path))
      | _ -> ok path)

let find_definition (ctx : ctx) (path : path) : (path, 'a) result =
  let open Monad.R in
  let ctx = handle_opens ctx in
  find_naive_definition ctx path |=> ensure_includes_correct ctx

let handle_file ctx file =
  let collect_names = raise (Failure "womp womp") in
  collect_names

let handle_files files =
  (* currently handles them in given order
     TODO: implement some sort of proper solver or something
  *)
  let ctx = { definitions = []; included = []; open' = []; current = "" } in
  let rec go files ctx =
    match files with
    | [] -> []
    | x :: xs ->
        let r, ctx = handle_file { ctx with current = x.name } x in
        r :: go xs ctx
  in
  go files ctx
