open IR

(* we can compute bound variable sets and
   do lambda lifting at the same time, if we're careful
   *)

let merge map1 map2 =
  NameMap.merge
    (fun a b c -> match b with Some x -> Some x | _ -> c)
    map1 map2

let ( let* ) x f =
  let expr, defs, frees = x in
  let expr', defs', frees' = f (expr, frees) in
  (expr', defs @ defs', frees')

let ( let+ ) expr f = f (expr, NameMap.empty)
let pure expr = (expr, [], NameMap.empty)

let fold_up (term : (expr * definition list * 'a NameMap.t) list) :
    expr list * definition list * 'a NameMap.t =
  let rec go = function
    | [] -> ([], [], NameMap.empty)
    | (e, d, s) :: xs ->
        let es, ds, ss = go xs in
        (e :: es, d @ ds, merge s ss)
  in
  go term

let rec without names name_map =
  match names with
  | [] -> name_map
  | x :: xs -> without xs (NameMap.remove x name_map)

let rec clos_conv_e free term =
  let* children, free' =
    List.map (clos_conv_e free) (get_children term) |> fold_up
  in
  let expr tag = Expr (get_data term, tag, children) in
  match term with
  | Expr (dat, Named (`Local, l), _) ->
      (term, [], NameMap.singleton l @@ get_typ term)
  | Expr (dat, Let nm, _) ->
      (expr (Let nm), [], NameMap.remove nm free')
  | Expr (dat, IfLet nm, _) ->
      (expr (IfLet nm), [], NameMap.remove nm free')
  | Expr (dat, Unpack (typs, nms), _) ->
      (expr (Unpack (typs, nms)), [], without nms free')
  | Expr (dat, Lambda (nm, typ), _) ->
      print_endline "doing lambda";
      let body = List.hd children in
      let def_name = fresh_name () in
      let free'' = NameMap.remove nm free' in
      let to_bind = NameMap.bindings free'' in
      List.iter (fun (a, b) -> print_endline (show_name a)) to_bind;
      let def =
        {
          name = def_name;
          (* needs to be last *)
          args = List.rev to_bind @ [ (nm, typ) ];
          returns = get_typ body;
          body;
        }
      in
      let rec make_function args fin =
        match args with
        | [] -> TyArrow (typ, fin)
        | (_, ty) :: xs -> TyArrow (ty, make_function xs fin)
      in
      let rec gen_apps rest body =
        match rest with
        | [] -> body
        | x :: xs ->
            Expr
              ( data' (),
                Funccall,
                [
                  gen_apps xs body;
                  Expr (data' (), Named (`Local, x), []);
                ] )
      in
      let typ = make_function (List.rev to_bind) @@ get_typ body in
      ( gen_apps (List.map fst to_bind)
          (Expr (data_with_typ typ, Named (`Global, def_name), [])),
        [ def ],
        free'' )
  | _ -> (expr (get_tag term), [], free')

let clos_conv prog =
  let new_defs =
    List.fold_left
      (fun acc def ->
        let body', defs, _ = clos_conv_e NameSet.empty def.body in
        ({ def with body = body' } :: defs) @ acc)
      [] prog.defs
  in
  { prog with defs = new_defs }
