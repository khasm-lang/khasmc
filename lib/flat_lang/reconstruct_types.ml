open IR

(* we need to ensure names are bound properly
   before the bodies are
   *)
let rec do_names nm_map expr =
  let open List in
  let go = reconstruct_e nm_map in
  match expr with
  | Expr (dat, Let nm, [a; _])
  | Expr (dat, IfLet nm, [a; _; _]) ->
    go a;
    NameMap.add nm (get_typ a) nm_map
  | Expr (dat, Lambda (nm, input), _) ->
    NameMap.add nm input nm_map
  | Expr (dat, Unpack (typs, nms), _) ->
    NameMap.add_seq (List.to_seq @@ List.combine nms typs) nm_map
  | _ -> nm_map

and reconstruct_e (nm_map : 'a NameMap.t) (expr : expr) : unit =
  let open List in
    let nm_map = do_names nm_map expr in
    let go = reconstruct_e nm_map in
    let map' f x = ignore @@ map f x in
    map' go (get_children expr);
  match expr with
  | Expr (dat, tag, children) when dat.typ <> TyUnknown -> ()
  | Expr (dat, Named (`Local, nm), _) ->
    begin match NameMap.find_opt nm nm_map with
    | Some ty ->
      dat.typ <- ty
    | None ->
      (* TODO: ensure this is never called *)
      print_endline "UNKNOWN LOCAL:";
      print_endline (show_expr expr);
      print_endline "bindings:";
      List.iter (fun (nm, ty) ->
        print_endline (show_name nm ^ ": " ^ show_typ ty)
      ) @@ NameMap.bindings nm_map;
      failwith "unknown local"
    end 
  | Expr (dat, Tuple, ts) -> dat.typ <- TyTuple (map get_typ ts)
  | Expr (dat, BinOp op, ts) ->
      if mem op [ Eq; Lt; Gt; LtEq; GtEq ] then
        dat.typ <- TyBase `Bool
      else
        dat.typ <- get_typ @@ hd ts
  | Expr (dat, UnaryOp Ref, ts) -> dat.typ <- TyRef (get_typ @@ hd ts)
  | Expr (dat, UnaryOp (Project i), ts) ->
      let[@warning "-8"] (TyTuple typs) = get_typ @@ hd ts in
      dat.typ <- nth typs i
  | Expr (dat, Lambda (_, input), ts) ->
      dat.typ <- TyArrow (input, get_typ @@ hd ts)
  | Expr (dat, Funccall, ts) ->
    begin match get_typ @@ hd ts with
    | TyArrow (l, r) -> 
      dat.typ <- r
    | x ->
      print_endline "bad function type: ";
      print_endline (show_typ x);
      print_endline "kids:";
      List.iter (fun x -> print_endline (show_expr x)) (get_children expr);
      failwith "bad"
    end
  | Expr (dat, Unpack (_, _), _) ->
    dat.typ <- TyTuple []
  | Expr (dat, Let _, [_; bd])
  | Expr (dat, IfLet _, [_; bd; _]) ->
        dat.typ <- get_typ bd
  | Expr (dat, Seq, [_; b]) ->
    dat.typ <- get_typ b
  | Expr (dat, Modify _, _) ->
    dat.typ <- TyTuple []
  | Expr (dat, Fail _, _) ->
    dat.typ <- TyBase `Bottom
  | Expr (dat, _, _) -> (
      (* all other tags should have types attached already...?
       TODO: verify
       *)
      dat.typ
      |> fun x ->
      match x with
      | TyUnknown ->
        print_endline "TyUnknown bad:";
        print_endline (show_expr expr);
        failwith "should not be unknown"
      | x -> ())


let reconstruct top = IR.process_in_definitions'
  (fun def x ->
    let map = NameMap.of_list def.args in
    reconstruct_e map x; x) top 
