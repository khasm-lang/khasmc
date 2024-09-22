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

type ctx = {
  traits : resolved trait list;
  methods : (resolved * resolved trait) list;
  impls : resolved impl list;
  by_trait : (resolved trait * resolved impl list) list;
  local_polys : resolved list;
}

let impl_collection : resolved impl by_uuid = new_by_uuid 100

let rec search_impls (ctx : ctx) (want : 'a trait_bound) :
    ('a impl, string) result =
  (* this function searches the context, trying to find a valid
     impl for some given trait bound
     also handles recursively trying to find all of the "lower"
     impls

     TODO: does this actually handle
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
                (* we want to figure out if we can find an
                   impl that matches the given constraints that we have
                *)
                let twice f (a, b) = (f a, f b) in
                let* args' = zipby args impl.args in
                let* assocs' = zipby assocs impl.assocs in
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
                  (* make sure to account for the fact that the
                     impl might also have "loose" polys lying around
                     for example in the case of
                     impl<a> foo<a> {}
                     and those are rigid

                     TODO: this resolution does not handle said cases
                     properly, as we have no way of expressing that
                     these are valid to unify with other rigid
                     polys, but not with anything else
                  *)
                  make_metas (ctx.local_polys @ impl.polys) all_metas
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
                *)
                failwith "tmp"
          in
          failwith "tmp")
