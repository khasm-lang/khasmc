open IR

let p x = "(" ^ x ^ ")"

let arr_of x = "[" ^ String.concat "," x ^ "]"

let show (nm : name) =
    match nm with
  R x ->
  "k_" ^ (if x < 0 then
  "m" ^ string_of_int (-x)
  else string_of_int x)

let build_ctor num_args idx =
  let i = string_of_int in 
  let rec go_a n =
    if n = 0 then ""
    else ("c" ^ i n ^ " => " ^ go_a (n - 1))
  in
  let rec go_b n =
    if n = 0 then ""
    else (i (num_args - n) ^ ": c" ^ i n ^ ", " ^ go_b (n - 1))
  in
  go_a num_args ^ "{return {" ^ go_b num_args ^ "\"tag\": " ^ i idx ^ "};}"

let rec emit_e ctors (expr : expr) =
  let go x = p (emit_e ctors x) in
  match expr with
  | Expr (dat, Fail _, args) -> "(() => {throw \"fail\"})()"
  | Expr (dat, Extern nm, args) -> nm
  | Expr (dat, Named (`Constructor args, nm), []) ->
       (* we only need args to know _how many_ *)
    let tag = Hashtbl.find ctors nm in
    build_ctor (List.length args) tag
  | Expr (dat, Named (_, nm), args) -> show nm
  | Expr (dat, Prim (_, nm), args) -> nm
  | Expr (dat, Tuple, args) -> arr_of (List.map go args) 
  | Expr (dat, BinOp Add, [a;b]) -> go a ^ "+" ^ go b
  | Expr (dat, BinOp Sub, [a;b]) -> go a ^ "-" ^ go b
  | Expr (dat, BinOp Mul, [a;b]) -> go a ^ "*" ^ go b
  | Expr (dat, BinOp Div, [a;b]) -> go a ^ "/" ^ go b
  | Expr (dat, BinOp LAnd, [a;b]) -> go a ^ "&&" ^ go b
  | Expr (dat, BinOp LOr, [a;b]) -> go a ^ "||" ^ go b
  | Expr (dat, BinOp Lt, [a;b]) -> go a ^ "<" ^ go b
  | Expr (dat, BinOp Gt, [a;b]) -> go a ^ ">" ^ go b
  | Expr (dat, BinOp LtEq, [a;b]) -> go a ^ "<=" ^ go b
  | Expr (dat, BinOp GtEq, [a;b]) -> go a ^ ">=" ^ go b
  | Expr (dat, BinOp Eq, [a;b]) -> go a ^ "==" ^ go b
  | Expr (dat, UnaryOp Negate, [a]) -> "-" ^ go a
  | Expr (dat, UnaryOp BNegate, [a]) -> "!" ^ go a
  | Expr (dat, UnaryOp Ref, args) -> failwith "ref"
  | Expr (dat, UnaryOp (Project i), [a])
    -> p (go a) ^ "[" ^ string_of_int i ^ "]"
  | Expr (dat, Lambda (_, _), args) -> failwith "lambda"
  | Expr (dat, Funccall, [a;b]) -> p (go a) ^ go b
| Expr (dat, Unpack (_, nms), [a;b]) ->
  let ga = go a in
  List.mapi (fun i nm -> "let " ^ show nm ^ " = " ^ ga ^ "["
    ^ string_of_int i ^ "]; " ) nms
    |> fun x ->
      p ("() => {" ^ String.concat "" x ^ " return " ^ go b ^ ";}") ^ "()"
  | Expr (dat, Let nm, [a;b]) ->
    p (p (show nm ^ " => " ^ go b) ^ (go a))
  | Expr (dat, IfLet nm, [a;b;c]) ->
    p ("() => { if (" ^ go a ^ "[\"tag\"] == " ^ string_of_int (Hashtbl.find ctors nm) ^ ") {"
    ^ "return " ^ go b ^ "; } else { return " ^ go c ^ "; } }"
    ) ^ "()"
  | Expr (dat, IfConst (_, i), [a;b;c]) ->
    p ("() => {if (" ^ go a ^ " == " ^ i ^ ") { return " ^ go b ^ "; } else
    { return " ^ go c ^ "; }}") ^ "()"
  | Expr (dat, Seq, args)
  | Expr (dat, Modify _, args) ->
      failwith "tmp"
  | _ -> failwith  "invalid input"


let emit top =
  List.iter (fun def ->
    print_endline "// BODY FOR: ";
    print_endline ("//" ^ show_name def.name);
    print_string ("let " ^ show def.name ^ " = (");
    List.iter (fun (e,_) -> print_string (show e ^ " => ")) def.args;
    print_string (emit_e top.constructors def.body);
    print_endline ");"
  ) top.defs 
