open Ast
open Exp

let (--) i j = 
    let rec aux n acc =
      if n < i then acc else aux (n-1) (n :: acc)
    in aux j []

let table =
  0 -- 255
  |> List.map (Printf.sprintf "%2X")

type ctx = {
    (* (name, mangled) *)
    binds : (string * string) list;
    namespace : string;
  }
[@@deriving show {with_path = false}]

let merge a b = {a with binds = a.binds @ b.binds}

let ctx_with n = {binds = [
                    ("()", "")
                  ]; namespace = n}

let ctx_add ctx s = {ctx with binds = s :: ctx.binds}

let ctx_get ctx id =
  List.find_opt (fun x -> fst x = id) ctx.binds

let string_of_chars chars = 
  let buf = Buffer.create 16 in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf

let chars_of_string s =
  List.init (String.length s) (String.get s)

let mangle_c c = List.nth table c

let mangle str =
  chars_of_string str
  |> List.map Char.code
  |> List.map mangle_c
  |> String.concat ""
  |> (^) "Khasmc_"

let mbox s = "[" ^ "\"" ^ mangle s ^ "\"" ^ "]"

let paren s = "(" ^ s ^ ")"

let square s = "[" ^ s ^ "]"

let kha_prefix = "_K"

let kprefix t = kha_prefix ^ square ("\"" ^ t ^ "\"")

let rec gen_lua_binops l =
  match l with
  | [] -> ""
  | x :: xs ->
     (fst x)
     ^  {| = function(a) return function(b) return a |} ^ snd x ^ {| b end end
                                                               |}
     ^ gen_lua_binops xs

let prelude = kha_prefix ^
                {| = {}
                 _K["if"] = function(c, e1, e2)
                 if c then return e1() else return e2() end
                 end

_K[";"] = function(a, b) a(); return b() end
                 |}
                ^
                  gen_lua_binops [
                      ("_Kadd", "+");
                      ("_Ksub", "-");
                      ("_Kdiv", "/");
                      ("_Kmul", "*");
                      ("_Kpow", "^");
                      ("_Keq", "==")
                    ]
                ^ {|

                   -- https://stackoverflow.com/questions/9168058/how-to-dump-a-table-to-console
                   function dump(o)
                   if type(o) == 'table' then
                   local s = '{ '
                   for k,v in pairs(o) do
                   if type(k) ~= 'number' then k = '"'..k..'"' end
                   s = s .. '['..k..'] = ' .. dump(v) .. ','
                   end
                   return s .. '} '
                   else
                   return tostring(o)
                   end
                   end
                   function _Kshow(x) print(dump(x)) end
                   
                   |}

let postlude =
  "\n\n\n" ^ kprefix (mangle "main") ^ "()"

let maybe_bottom s =
  if s = "()" then "" else s

(*

  TODO:
  Ad Hoc Poly, both here and in the typechecker

 *)

let rec codegen_base ctx e =
  match e with
  | Ident(t) ->
     begin
       match ctx_get ctx t with
       | Some(x) ->
          maybe_bottom (snd x)
       | None -> t 
     end
  | Int(i)
    | Float(i)
    -> i
  | Str(s) -> "\"" ^ s ^ "\""
  | Tuple(t) -> "({"
                ^ String.concat ", " (List.map (codegen_expr ctx) t)
                ^ "})"
  | True -> "true"
  | False -> "false"


and codegen_expr ctx expr =
  match expr with
  | Base(x) -> codegen_base ctx x
  | FCall(a, b) ->
     paren (codegen_expr ctx a) ^ paren (codegen_expr ctx b)
  | AnnotLet(a, _, e1, e2) 
    | LetIn(a, e1, e2) ->
     a ^ " = " ^ codegen_expr ctx e1
     ^ "; " ^ codegen_expr ctx e2
  | IfElse(c, e1, e2) ->
     (* use the _K["if"] helper in the prelude *)
     kha_prefix ^ {|["if"]( |} ^ paren (codegen_expr ctx c)
     ^ ", function() return " ^ (codegen_expr ctx e1)
     ^ " end, function() return " ^ (codegen_expr ctx e2)
     ^ " end )"
     
  | Join(a, b) ->
     (kha_prefix ";")
     ^ "(" 
     ^ (codegen_expr ctx a)
     ^ ", " 
     ^ (codegen_expr ctx b)
     ^ ")"
  | Inst(_, _) ->
     raise (Impossible "INST codegen_expr")
  | AnnotLam(i, _, e)
    | Lam(i, e) ->
     "function" ^ paren (maybe_bottom i) ^ " return " ^ codegen_expr ctx e ^ " end"
  | TypeLam(_, e) ->
     codegen_expr ctx e
  | TupAccess(e, i) -> paren (codegen_expr ctx e) ^ "["
                       ^ string_of_int (i + 1) ^ "]"

let rec codegen_assign ctx id args bd =
  let rec helper args bd =
    match args with
    | [] -> bd
    | x :: xs -> Lam(x, helper xs bd)
  in
  let proper = helper args bd in
  kha_prefix ^ mbox id ^ " = " ^ codegen_expr ctx proper
  

  
let rec codegen_toplevel ctx tp =
  match tp with
  | [] -> ("", ctx)
  | x :: xs ->
     let (s, ctx') = begin
       match x with
       | Extern(id, ts) ->
          ("\n-- EXTERN " ^ id ^ " : " ^ pshow_typesig ts ^ "\n"
          , ctx_add ctx (id, id))
       | TopAssign((id, ts), (_, args, bd)) ->
          ("\n-- TOPASSIGN " ^ id ^ " : " ^ pshow_typesig ts ^ "\n"
           ^ codegen_assign ctx id args bd
           , ctx_add ctx (id, kprefix(mangle id)))
       end in
     let next = codegen_toplevel ctx' xs in
     (s ^ fst next, merge (snd next) ctx')

let rec codegen_program ctx file =
  match file with
  | Program(x) -> codegen_toplevel ctx x

let rec codegen_h names files ctx =
  let ctx = match ctx with
    | Some(x) -> x
    | None -> ({binds=[]; namespace=""})
  in
  match (names, files) with
  | ([], []) -> "-- END"
  | (n :: ns, f :: fs) ->
     let fctx = codegen_program (merge (ctx_with n) ctx) f in
     "-- BEGIN FILE " ^ n ^ "\n"
     ^ fst fctx
     ^ "\n-- END FILE " ^ n
     ^ "\n\n" ^ codegen_h ns fs (Some(snd fctx))
  | (_, _) -> raise (Impossible "codegen_h")

let codegen names files =
  prelude ^ codegen_h names files None ^ postlude
