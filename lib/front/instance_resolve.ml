open Ast
open Common.Info
open Common.Error
open Common
open Tycheck

type info += Traitfn_inst
type data += Traitfn_inst' of id option

type ctx = {
  trait_fns : (string * trait) list;
  impls : impl list;
}

let empty_ctx () = { trait_fns = []; impls = [] }

let add_traitfn ctx fnname trait =
  { ctx with trait_fns = (fnname, trait) :: ctx.trait_fns }

let add_impl ctx impl = { ctx with impls = impl :: ctx.impls }

let match_solve_frees args fnty traitty =
  let inst = Unify.inst_frees args fnty in
  let pairs = ref [] in
  let cont (a : ty) (b : ty) : ty =
    match (a, b) with
    | x, Free s ->
        pairs := (s, x) :: !pairs;
        x
    | _ -> raise (Unify.BadUnify (a, b))
  in
  Log.trace (show_ty inst);
  Log.trace (show_ty traitty);
  ignore @@ Unify.unify' cont inst traitty;
  List.iter (fun (a, b) -> Log.trace (a ^ " & " ^ show_ty b)) !pairs;
  !pairs

let solve ctx id fnname ty trait =
  let fn =
    List.find
      (fun (d : definition_no_body) -> d.name = fnname)
      trait.functions
  in
  let t = mk_ty (List.map snd fn.args) fn.ret in
  let pairs = match_solve_frees trait.args ty t in
  if
    not
      (List.for_all (( = ) true)
      @@ List.map (fun x -> List.mem (fst x) trait.args) pairs)
  then
    Log.fatal "we don't support non-full inference at this time"
  else
    let sorted = List.sort (fun (a, _) (b, _) -> compare a b) pairs in
    match
      List.find_opt
        (fun (i : impl) ->
          let args =
            List.sort (fun (a, _) (b, _) -> compare a b) i.args
          in
          Log.debug (string_of_int @@ List.length sorted);
          Log.debug (string_of_int @@ List.length args);
          List.map2
            (fun (_, pair) (_, inst) -> Unify.unify_b [] pair inst)
            sorted args
          |> List.mem false
          |> not)
        ctx.impls
    with
    | None -> Log.fatal "no instance found"
    | Some s ->
        Log.trace "found:";
        Log.trace (Ast.show_impl s);
        set_property id Traitfn_inst (Traitfn_inst' (Some s.id))

(* trait fn names & impls *)
let gather_needed_info (stmts : statement list) : ctx =
  List.fold_left
    (fun acc x ->
      match x with
      | Trait t ->
          List.fold_left
            (fun acc (fn : definition_no_body) ->
              add_traitfn acc fn.name t)
            acc t.functions
      | Impl t -> add_impl acc t
      | _ -> acc)
    (empty_ctx ()) stmts
