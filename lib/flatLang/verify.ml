open IR

let rec all (b : bool list) : bool =
  match b with [] -> true | x :: xs -> x && all xs

let rec verify_e num (e : expr) : bool =
  let f x = all (List.map (verify_e num) x) in
  let v = verify_e num in
  if get_typ e = TyUnknown then
    incr num
  else ();
  try
    match e with
    | Expr (_, Fail _, []) -> true
    | Expr (_, Named (_, _), []) -> true
    | Expr (_, Prim (_, _), []) -> true
    | Expr (_, Bool _, []) -> true
    | Expr (_, Tuple, children) -> f children
    | Expr (_, BinOp _, [ a; b ]) -> v a && v b
    | Expr (_, UnaryOp _, [ a ]) -> v a
    | Expr (_, Lambda _, [ a ]) -> v a
    | Expr (_, Funccall, children) -> f children
    | Expr (_, Let _, [ a; b ]) -> v a && v b
    | Expr (_, IfLet _, [ a; b; c ]) -> v a && v b && v c
    | Expr (_, Seq, children) -> f children
    | Expr (_, Modify _, [ m ]) -> v m
    | Expr (_, Unpack _, [ a; b ]) -> v a && v b
    | _ -> failwith "No match"
  with _ ->
    print_endline "VERIFY FAILED:";
    print_endline ("expr: \n" ^ show_expr e ^ "\n is invalid");
    failwith "verify failed"

let verify top =
  let num = ref 0 in
  let res = List.for_all (fun d -> verify_e num d.body) top.defs in
  print_endline ("verify: unknown type nodes: " ^ string_of_int !num);
  Complexity.complexity top;
  res
