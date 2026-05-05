open IR

let rec all (b : bool list) : bool =
  match b with [] -> true | x :: xs -> x && all xs

let rec verify_e num vars (e : expr) : bool =
  let f x = all (List.map (verify_e num vars) x) in
  let v = verify_e num vars in
  let go = verify_e num in
  if get_typ e = TyUnknown then
    incr num
  else ();
  try
    match e with
    | Expr (_, Fail _, []) -> true
    | Expr (_, Named (`Local, l), []) ->
      if not @@ NameSet.mem l vars then begin
        print_endline ("variable " ^ show_name l ^ " unknown verify");
        false end
      else true
    | Expr (_, Named (_, _), []) -> true
    | Expr (_, Prim (_, _), []) -> true
    | Expr (_, Bool _, []) -> true
    | Expr (_, Tuple, children) -> f children
    | Expr (_, BinOp _, [ a; b ]) -> v a && v b
    | Expr (_, UnaryOp _, [ a ]) -> v a
    | Expr (_, Lambda (l, _), [ a ]) ->
      let n = NameSet.add l vars in
      go n a
    | Expr (_, Funccall, children) -> f children
    | Expr (_, Let nm, [ a; b ]) ->
      let n = NameSet.add nm vars in
      v a && go n b
    | Expr (_, IfLet nm, [ a; b; c ]) ->
      let n = NameSet.add nm vars in
      v a && go n b && go n c
    | Expr (_, Seq, children) -> f children
    | Expr (_, Modify _, [ m ]) -> v m
    | Expr (_, Unpack (_,nms), [ a; b ]) ->
      let n = NameSet.add_seq (List.to_seq nms) vars in
      v a && go n b
    | _ -> failwith "No match"
  with _ ->
    print_endline "VERIFY FAILED:";
    print_endline ("expr: \n" ^ show_expr e ^ "\n is invalid");
    failwith "verify failed"

let verify loud top =
  let num = ref 0 in
  let res = List.for_all (fun d -> verify_e num (NameSet.of_list (List.map fst d.args)) d.body) top.defs in
  if loud then begin
    print_endline ("verify: unknown type nodes: " ^ string_of_int !num);
    Complexity.complexity_verbose top;
  end;
  res
