open Typecheck
open Ast
open Share.Uuid
open Share.Result
open Unify

let zipby (l1 : ('a * 'b) list) (l2 : ('a * 'c) list) :
    (('b * 'c) list, string) result =
  let h (a, _) (b, _) = compare a b in
  let l1 = List.sort h l1 in
  let l2 = List.sort h l2 in
  let rec go l1 l2 =
    match (l1, l2) with
    | (x, a) :: xs, (y, b) :: ys when x = y ->
        let* rest = go xs ys in
        ok @@ ((a, b) :: rest)
    | [], [] -> ok []
    | _ :: _, [] | [], _ :: _ -> err "lists not equal length zipby"
    | _ -> err "keys/vals don't match up zipby"
  in
  go l1 l2

type resolved_by =
  | Global of resolved impl
  | Local of resolved * resolved trait_bound (* type, trait *)

type ctx = {
  traits : resolved trait list;
  methods : (resolved * resolved trait) list;
  impls : resolved_by list;
  by_trait : (resolved trait * resolved_by list) list;
  local_polys : resolved list;
}

let impl_collection : resolved_by by_uuid = new_by_uuid 100

let rec search_impls (ctx : ctx) (want : 'a trait_bound) :
    (resolved_by, string) result =
  (* this function searches the context, trying to find a valid
     impl for some given trait bound
     does NOT try and find "lower" dependency impls
  *)
  let name, args, assocs = want in
  match
    List.find_opt (fun (x : 'a trait) -> x.name = name) ctx.traits
  with
  | None -> err @@ "can't find trait " ^ show_resolved name
  | Some trait -> (
      match List.find_opt (fun (a, b) -> a = trait) ctx.by_trait with
      | None -> err @@ "can't find impls for " ^ show_resolved name
      | Some (_, impls) ->
          let rec go xs =
            match xs with
            | [] -> err "no matching impl found!"
            | impl :: xs ->
                let impl_args, impl_assocs =
                  match impl with
                  | Global i -> (i.args, i.assocs)
                  | Local (_, (_, args, assocs)) -> (args, assocs)
                in
                (* we want to figure out if we can find an
                   impl that matches the given constraints that we have
                *)
                let twice f (a, b) = (f a, f b) in
                let* args' = zipby args impl_args in
                let* assocs' = zipby assocs impl_assocs in
                (* copy to ensure no "cross influences" *)
                let args' = List.map (twice copy_typ) args' in
                let assocs' = List.map (twice copy_typ) assocs' in
                (* TODO: expensive *)
                let all_metas =
                  [
                    List.map
                      (fun (a, b) -> get_polys a @ get_polys b)
                      args';
                    List.map
                      (fun (a, b) -> get_polys a @ get_polys b)
                      assocs';
                  ]
                  |> List.flatten
                  |> List.flatten
                in
                let map =
                  (* make sure to keep local rigids rigid *)
                  make_metas ctx.local_polys all_metas
                in
                (* turn all the polys that aren't rigid into metas *)
                let args'inst =
                  List.map (twice (instantiate map)) args'
                in
                let assocs'inst =
                  List.map (twice (instantiate map)) assocs'
                in
                let* _ =
                  List.map (fun (a, b) -> unify_h a b) args'inst
                  |> collect
                  |> Result.map_error (String.concat "\n")
                in
                let* _ =
                  List.map (fun (a, b) -> unify_h a b) assocs'inst
                  |> collect
                  |> Result.map_error (String.concat "\n")
                in
                (* if we get to here, everything unified nicely, so
                   we have a match!

                   now we need to use the map that we got earlier,
                   take the trait that we're dealing with, and
                   find impls for all of the traits that are bounds
                   for this one
                *)
                ok @@ impl
          in
          go impls)
