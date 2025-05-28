open Frontend.Typecheck
open Frontend.Ast
open Share.Uuid
open Share.Result
open Share.Maybe
open Frontend.Unify

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
  (* functions with trait bounds *)
  has_bounds : (resolved * resolved trait_bound list) list;
}
[@@deriving show { with_path = false }]

let has_bounds ctx id = List.assoc_opt id ctx.has_bounds

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
            has_bounds =
              (* for each function, we add its required bounds
                 (as we allow functions to add on extra bounds if
                 they so wish (TODO: is this valid????))

                 then, we add "itself" as a bound - this way, if
                 something computes the bounds of, say, show, they
                 get Show T as the "primary" bound, which is exactly
                 what we want them to solve for
              *)
              (let f = List.map (fun x -> (x, TyPoly x)) in
               List.map
                 (fun (a : ('a, 'b) definition) ->
                   let t : resolved trait_bound =
                     (uuid (), t.name, f t.args, f t.assocs)
                   in
                   (a.name, t :: a.bounds))
                 t.functions
               @ acc.has_bounds);
          }
      | Impl i -> { acc with impls = Global i :: acc.impls }
      | Definition d ->
          if d.bounds = [] then
            acc
          else
            {
              acc with
              has_bounds = (d.name, d.bounds) :: acc.has_bounds;
            })
    {
      traits = [];
      methods = [];
      impls = [];
      local_polys = [];
      has_bounds = [];
    }
    top

type solved =
  (* bound solved, how we solved it, all of the "subproblems" *)
  | Solution of uuid * resolved_by * solved list
[@@deriving show { with_path = false }]

(* the allmighty map of uuid -> solved trait bounds *)
let trait_information : (uuid, solved list) Hashtbl.t =
  new_by_uuid 100

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
  let* impls =
    match impls_by_trait ctx trait with
    | [] -> err @@ "can't find impls for " ^ show_resolved name
    | xs -> ok xs
  in
  let rec go xs =
    match xs with
    | [] ->
        err
          ("no matching impl found!"
          ^ "\nbound: "
          ^ show_trait_bound pp_resolved want)
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
          (* force to ensure no "cross influences" (PLACEBO) *)
          let args' = List.map (twice force) args' in
          let assocs' = List.map (twice force) assocs' in
          let get_both_polys (a, b) = get_polys a @ get_polys b in
          let all_metas =
            ctx.local_polys
            @ List.map fst impl_args
            @ List.map fst impl_assocs
            @ (List.flatten @@ List.map get_both_polys args')
            @ List.flatten
            @@ List.map get_both_polys assocs'
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
          (* also check requirements *)
          let trait_subproblems = trait.requirements in
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
        | Ok sol -> begin
            match go xs with
            | Error _ -> ok sol
            | Ok _ -> err "multiple solutions! no bueno :("
          end
        | Error e -> go xs)
  in
  let* sol = go impls in
  ok sol

let solve_all_bounds_for (ctx : ctx) (uuid : uuid) (e : resolved)
    (bounds : resolved trait_bound list) : (unit, string) result =
  (*
    So: We know that we're solving for <e>, and that it's part of
    trait <trt>. We can fetch the type information for <e>, which
    we need to do in order to figure out what instance we want to
    search for. So, we grab that type info, "match everything up"
    via taking the expected type of the function v.s. the trait's
    version of the type to generate a set of constraints like so:

    in show 5, show : Int -> String

    but in
    trait Show T =
    show : T -> String
    end

    show : T(bound) -> String

    so we generate T = Int (nothing fancy - just grabbing eagerly)

    and solve for Show String, which then gives us a Solution that
    we link to the uuid of the resolved.
   *)
  let* exp_typ =
    match Hashtbl.find_opt raw_type_information e with
    | Some s -> Ok (force s)
    | None -> Error ("No raw type info found for" ^ show_resolved e)
  in
  let* real_typ =
    match Hashtbl.find_opt type_information uuid with
    | Some s -> Ok (force s)
    | None -> Error ("No type info found for" ^ show_resolved e)
  in
  let* trait =
    match List.assoc_opt e ctx.methods with
    | Some t -> ok t
    | None -> Error ("No trait for " ^ show_resolved e)
  in
  let all_polys = trait.args @ trait.assocs in
  let rec go real exp =
    match (force real, force exp) with
    | a, b when a = b -> []
    | a, TyPoly b when List.mem b all_polys -> [ (b, a) ]
    | TyTuple a, TyTuple b -> List.flatten (List.map2 go a b)
    | TyArrow (a, b), TyArrow (q, w) ->
        (* LHS usually smaller than rhs*)
        go a q @ go b w
    | TyCustom (_, a), TyCustom (_, b) ->
        List.flatten (List.map2 go a b)
    | TyAssoc (a, _), TyAssoc (b, _) ->
        do_within_trait_bound'2 go a b |> List.flatten
    | TyRef a, TyRef b -> go a b
    | TyMeta a, TyMeta b when a = b -> []
    | _, _ -> failwith "impossible: solver.go no match"
  in
  let polys_to_reals = go real_typ exp_typ in
  let computed_bounds =
    List.map
      (do_within_trait_bound (subst_polys polys_to_reals))
      bounds
  in
  let* solutions =
    List.map (search_impls ctx) computed_bounds
    |> collect
    |> Result.map_error (String.concat "\n")
  in
  (* add to the allmighty trait information table *)
  Hashtbl.replace trait_information uuid solutions;
  ok ()

let rec resolve_expr (ctx : ctx) (e : resolved expr) :
    (unit, string) result =
  match e with
  | MLocal _ | MGlobal _ ->
      failwith "monomorphization info in trait resolution"
  | Var (d, id) -> begin
      match has_bounds ctx id with
      | Some bounds -> solve_all_bounds_for ctx d.uuid id bounds
      | None -> ok ()
    end
  | Int (_, _) -> ok ()
  | String (_, _) -> ok ()
  | Char (_, _) -> ok ()
  | Float (_, _) -> ok ()
  | Bool (_, _) -> ok ()
  | LetIn (_data, _case, _ty, head, body) ->
      let* _ = resolve_expr ctx head in
      let* _ = resolve_expr ctx body in
      ok ()
  | Seq (_, a, b) ->
      let* _ = resolve_expr ctx a in
      let* _ = resolve_expr ctx b in
      ok ()
  | Funccall (_, a, b) ->
      let* _ = resolve_expr ctx a in
      let* _ = resolve_expr ctx b in
      ok ()
  | Binop (_, op, a, b) ->
      let* _ = resolve_expr ctx a in
      let* _ = resolve_expr ctx b in
      ok ()
  | Lambda (_, id, _, e) -> resolve_expr ctx e
  | Tuple (_, es) ->
      List.map (resolve_expr ctx) es
      |> collect
      |> Result.map_error (String.concat "\n")
      |> Result.map (fun _ -> ())
  | Annot (_, e, _) -> resolve_expr ctx e
  | Match (_, _, xs) ->
      List.map (fun (c, e) -> resolve_expr ctx e) xs
      |> collect
      |> Result.map_error (String.concat "\n")
      |> Result.map (fun _ -> ())
  | Project (_, e, _) -> resolve_expr ctx e
  | Ref (_, r) -> resolve_expr ctx r
  | Modify (_, _, e) -> resolve_expr ctx e
  | Record (_, _, xs) ->
      List.map (fun (c, e) -> resolve_expr ctx e) xs
      |> collect
      |> Result.map_error (String.concat "\n")
      |> Result.map (fun _ -> ())

let resolve_definition (ctx : ctx) (d : (resolved, yes) definition) :
    (unit, string) result =
  (* The most pressing thing we have to deal with
     (as all global impls are already in the ctx)
     is adding our "local" impls in, so that they can be correctly
     resolved & tagged.

     TODO: handle that trait dependencies should be avaliable when
     doing resolution
  *)
  let bounds = d.bounds |> List.map (fun p -> Local p) in
  let ctx =
    {
      ctx with
      impls = bounds @ ctx.impls;
      local_polys = d.typeargs @ ctx.local_polys;
    }
  in
  resolve_expr ctx (get d.body)

let resolve_impl (ctx : ctx) (i : resolved impl) :
    (unit, string) result =
  List.map (fun (a, b) -> resolve_definition ctx b) i.impls
  |> collect
  |> Result.map_error (String.concat "\n")
  |> Result.map (fun _ -> ())

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
  |> function
  | Ok () ->
      print_newline ();
      print_endline "resolved :D";
      Ok ()
  | x -> x
