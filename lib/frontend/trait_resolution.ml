open Typecheck
open Ast
open Share.Uuid
open Share.Result
open Share.Maybe
open Unify

let zipby (l1 : ('a * 'b) list) (l2 : ('a * 'c) list) :
    (('b * 'c) list, string) result =
  if List.length l1 <> List.length l2 then
    err "lengths not equal zipby"
  else
    let h (a, _) (b, _) = compare a b in
    let l1 = List.sort h l1 in
    let l2 = List.sort h l2 in
    let rec go l1 l2 =
      match (l1, l2) with
      | (x, a) :: xs, (y, b) :: ys when x = y ->
          let* rest = go xs ys in
          ok @@ ((a, b) :: rest)
      | [], [] -> ok []
      | _ :: _, [] | [], _ :: _ -> failwith "impossible"
      | _ -> err "keys/vals don't match up zipby"
    in
    go l1 l2

type resolved_by =
  | Global of resolved impl
  | Local of resolved trait_bound (* trait bound *)
[@@deriving show { with_path = false }]

type ctx = {
  (* traits *)
  traits : resolved trait list;
  (* method / trait with method *)
  methods : (resolved * resolved trait) list;
  (* impls *)
  impls : resolved_by list;
  (* any local polys (needed?) *)
  local_polys : resolved list;
}

let impls_by_trait (c : ctx) (i : resolved trait) : resolved_by list =
  List.filter
    (fun t ->
      match t with
      | Global g -> g.parent = i.name
      | Local (_, id, _, _) -> id = i.name)
    c.impls

let build_ctx top =
  List.fold_left
    (fun acc -> function
      | Typdef _ -> acc
      | Trait t ->
          {
            acc with
            traits = t :: acc.traits;
            methods =
              List.map
                (fun (a : ('a, 'b) definition) -> (a.name, t))
                t.functions
              @ acc.methods;
          }
      | Impl i -> { acc with impls = Global i :: acc.impls }
      | Definition _ -> acc)
    { traits = []; methods = []; impls = []; local_polys = [] }
    top

type solved =
  (* bound solved, how we solved it, all of the "subproblems" *)
  | Solution of uuid * resolved_by * solved list
[@@deriving show { with_path = false }]

let impl_collection : solved by_uuid = new_by_uuid 100

let rec search_impls (ctx : ctx) (want : 'a trait_bound) :
    (solved, string) result =
  (* this function searches the context, trying to find a valid
     impl for some given trait bound
  *)
  let unique, name, args, assocs = want in
  let* trait =
    List.find_opt (fun (x : 'a trait) -> x.name = name) ctx.traits
    |> function
    | None -> err @@ "can't find trait " ^ show_resolved name
    | Some n -> ok n
  in
  print_endline "trait:";
  print_endline (show_trait pp_resolved trait);
  let* impls =
    match impls_by_trait ctx trait with
    | [] -> err @@ "can't find impls for " ^ show_resolved name
    | xs -> ok xs
  in
  let rec go xs =
    match xs with
    | [] -> err "no matching impl found!"
    | impl :: xs -> (
        match
          let uuid, impl_args, impl_assocs =
            match impl with
            | Global i -> (i.data.uuid, i.args, i.assocs)
            | Local (uuid, _, args, assocs) -> (uuid, args, assocs)
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
              List.map (fun (a, b) -> get_polys a @ get_polys b) args';
              List.map
                (fun (a, b) -> get_polys a @ get_polys b)
                assocs';
              List.map fst impl_args :: [];
              List.map fst impl_assocs :: [];
            ]
            |> List.flatten
            |> List.flatten
          in
          let map =
            (* make sure to keep local rigids rigid *)
            make_metas ctx.local_polys all_metas
          in
          (* prematurely solve all of the arguments and assocs
             all of these are new metas we just made with make_metas,
             so they should all be completely fresh

             if they have a matching type in the set of args, we
             eagerly inst that
          *)
          let args_assocs = impl_args @ impl_assocs in
          List.iter
            (fun (name, typ) ->
              match (List.assoc_opt name args_assocs, force typ) with
              (* inst the metas *)
              | Some t, TyMeta m -> begin
                  match !m with
                  | Resolved _ -> failwith "impossible"
                  | Unresolved ->
                      m := Resolved t;
                      ()
                end
              (* otherwise we're fine *)
              | _ -> ())
            map;
          (* turn all the polys that aren't rigid into metas *)
          let args'inst = List.map (twice (instantiate map)) args' in
          let assocs'inst =
            List.map (twice (instantiate map)) assocs'
          in
          (* grab errors *)
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
          List.iter
            (fun (R i, a) ->
              print_int i;
              print_string " = ";
              typ_pp a)
            map;
          (* also check requirements *)
          let trait_subproblems = trait.requirements in
          List.iter
            (fun a -> print_endline (show_trait_bound pp_resolved a))
            trait_subproblems;
          (* make sure to instantiate all of those tyvars *)
          let f = List.map (fun (a, b) -> (a, instantiate map b)) in
          let subproblems_inst : 'a trait_bound list =
            trait_subproblems
            |> List.map (fun (u, i, args, assocs) ->
                   (u, i, f args, f assocs))
          in
          (* try all the subproblems *)
          let* attempts =
            subproblems_inst
            |> List.map (search_impls ctx)
            |> collect
            |> Result.map_error (String.concat "\n")
          in

          ok @@ Solution (unique, impl, attempts)
        with
        | Ok sol ->
            (* TODO: check we don't get multiple solutions? *)
            ok sol
        | Error e -> go xs)
  in
  let* sol = go impls in
  let (Solution (uuid, _, deps)) = sol in
  Hashtbl.add impl_collection uuid sol;
  ok sol

let resolve_expr (ctx : ctx) (e : resolved expr) :
    (unit, string) result =
  match e with
  | Var (_, _) -> failwith "tmp"
  | Int (_, _) -> failwith "tmp"
  | String (_, _) -> failwith "tmp"
  | Char (_, _) -> failwith "tmp"
  | Float (_, _) -> failwith "tmp"
  | Bool (_, _) -> failwith "tmp"
  | LetIn (_, _, _, _, _) -> failwith "tmp"
  | Seq (_, _, _) -> failwith "tmp"
  | Funccall (_, _, _) -> failwith "tmp"
  | Lambda (_, _, _, _) -> failwith "tmp"
  | Tuple (_, _) -> failwith "tmp"
  | Annot (_, _, _) -> failwith "tmp"
  | Match (_, _, _) -> failwith "tmp"
  | Project (_, _, _) -> failwith "tmp"
  | Ref (_, _) -> failwith "tmp"
  | Modify (_, _, _) -> failwith "tmp"
  | Record (_, _, _) -> failwith "tmp"

let resolve_definition (ctx : ctx) (d : (resolved, yes) definition) :
    (unit, string) result =
  (* The most pressing thing we have to deal with
     (as all global impls are already in the ctx)
     is adding our "local" impls in, so that they can be correctly
     resolvd & tagged.
  *)
  let bounds = d.bounds |> List.map (fun p -> Local p) in
  let ctx = { ctx with impls = bounds @ ctx.impls } in
  resolve_expr ctx (get d.body)

let resolve_impl (ctx : ctx) (i : resolved impl) :
    (unit, string) result =
  failwith "tmp"

let resolve (top : resolved toplevel list) : (unit, string) result =
  let ctx = build_ctx top in
  let rec go = function
    | Definition d -> resolve_definition ctx d
    | Impl i -> resolve_impl ctx i
    | _ -> ok ()
  in
  List.map go top
  |> collect
  |> Result.map (fun _ -> ())
  |> Result.map_error (String.concat "\n")
