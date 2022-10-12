open Kenv
open Print_ast
open Typecheck
open Ast
exception Impossible

type mangled = string * string

type codegenCtx = {
    parent : codegenCtx option;
    vars : var_t list;
    map_mangled : mangled list
  }

let rec codegenAddVar c v prefix =
  let tmp = {c with vars = v :: c.vars} in
  let attrs = codegenFindVar tmp v.id in
  {tmp with map_mangled = (v.id, getMangled v.id attrs prefix) :: tmp.map_mangled}

and codegenFindVarHelper vars v =
  match vars with
  | [] -> print_endline "empty vars"; print_endline v; raise Impossible
  | x :: xs ->
     if x.id = v then
       x
     else
       codegenFindVarHelper xs v

and codegenFindVar c v =
  codegenFindVarHelper c.vars v

and explode s = List.init (String.length s) (String.get s)

and char_list_to_hex cl =
  match cl with
  | [] -> ""
  | x :: xs ->
     Printf.sprintf "%.2X" (Char.code x) ^ char_list_to_hex xs

and hex_of_string s =
  let c = explode s in
  char_list_to_hex c

and mangle x =
  "K_IDENT_" ^ hex_of_string x

and isNoMangle attrlist =
  match attrlist with
  | [] -> false
  | x :: xs ->
     match x with
     | NoMangle(x) -> x
     | _ -> isNoMangle xs

and getMangled name attrs prefix =
  if isNoMangle attrs.at then
    name
  else
    if name = "main" then
      name
    else
      mangle (prefix ^ "::" ^ name)

and mangle_of_name x ctx =
  let m = ctx.map_mangled in
  match List.filter (fun y -> (fst y) = x) m with
  | [x] -> snd x
  | _ -> print_endline "multiple results for request of one function";
         raise Impossible

let codegenConst c ctx funccall =
  match c with
  | Int(x) -> x
  | Float(x) -> x
  | String(x) -> "\"" ^ x ^ "\""
  | Id(x) -> if funccall = true then mangle_of_name x ctx else x
  | True -> "true"
  | False -> "false"

let rec codegenUnOp ul x ctx =
  match ul with
  | [] -> codegenExpr x ctx false
  | y :: ys ->
     match y with
     | UnOpRef -> "[" ^ codegenUnOp ys x ctx ^ "]"
     | UnOpDeref -> "(" ^ codegenUnOp ys x ctx ^ ")[0]"
     | UnOpPos -> "Math.abs(" ^ codegenUnOp ys x ctx ^ ")"
     | UnOpNeg -> "-(" ^ codegenUnOp ys x ctx ^ ")"

and codegenBinOp x =
  match x with
  | BinOpPlus -> " + "
  | BinOpMinus -> " - "
  | BinOpMul -> " * "
  | BinOpDiv -> " / "

and codegenFuncArgs el ctx =
  match el with
  | [] -> ""
  | x :: xs -> "(" ^ codegenExpr x ctx false ^ ")" ^ codegenFuncArgs xs ctx

and codegenFuncCall e el ctx =
  codegenExpr e ctx true ^ codegenFuncArgs el ctx

and codegenExpr expr ctx funccall =
  match expr with
  | Paren(x) -> "(" ^ codegenExpr x ctx false ^ ")"
  | Base(x) -> codegenConst x ctx funccall
  | UnOp(ul, x) -> codegenUnOp ul x ctx
  | BinOp(a, e, b) -> codegenExpr a ctx false ^ codegenBinOp e ^ codegenExpr b ctx false
  | FuncCall(e, el) -> codegenFuncCall e el ctx

let rec codegenArgs args ctx =
  match args with
  | [] -> ""
  | x :: xs ->
     x ^ " => " ^ codegenArgs xs ctx


let codegenAssign name args expr prefix ctx =
  let attrs = codegenFindVar ctx name in
  let new_name = getMangled name attrs prefix in
  new_name
  ^ " = "
  ^ (codegenArgs args ctx)
  ^ (codegenExpr expr ctx false)
  ^ "\n" 
      
let codegenAssignBlock name args block prefix ctx =
  "/* NOTIMPL: AssignBlock */"

let codegenToplevel code prefix ctx =
  match code with
  | AssignBlock(i, args, block) -> codegenAssignBlock i args block prefix ctx
  | Assign(i, args, block) -> codegenAssign i args block prefix ctx
  | Typesig(a, _, _) -> "/* DEBUG: typesig for " ^ a ^ " */\n"

let rec codegenToplevelList code prefix ctx =
  let newctx =
    match ctx with
    | None -> {parent = None; vars = []; map_mangled = []}
    | Some(x) -> x
  in
  match code with
  | [] -> ""
  | x :: xs ->
     begin
       let childctx = 
         match x with
         | Typesig(name, ts, attr) -> codegenAddVar newctx {id=name;ts=ts;at=attr;} prefix
         | _ -> newctx
       in
       codegenToplevel x prefix childctx ^ codegenToplevelList xs prefix (Some(childctx))
     end

let codegenProgram file prog =
  let prefix = file in
  match prog with
  | Prog(x) -> codegenToplevelList x prefix None

let rec codegenProgramList files progs =
  match (files, progs) with
  | ([], []) -> []
  | (x :: xs, y :: ys) -> codegenProgram x y :: codegenProgramList xs ys
  | (_, _) -> print_endline "uneq number of files and programs"; raise Impossible

