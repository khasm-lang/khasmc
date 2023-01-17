open Hash
open Ast
open Exp
open Typecheck


let (--) i j = 
    let rec aux n acc =
      if n < i then acc else aux (n-1) (n :: acc)
    in aux j []

let table =
  0 -- 255
  |> List.map (Printf.sprintf "%2X")

let string_of_chars chars = 
  let buf = Buffer.create 16 in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf

let chars_of_string s =
  List.init (String.length s) (String.get s)

let mangle_c c = List.nth table c


let mangle str =
  if str = "()" then "()" else
    chars_of_string str
    |> List.map Char.code
    |> List.map mangle_c
    |> String.concat ""
    |> (^) "Khasmc_"


type scope = {
    binds: (string * string) list;
  }
[@@deriving show {with_path = false}]

let new_scope () =
  {binds = ("()", "()") :: []}

let add_bind scp id bd =
  {binds = (id, bd) :: scp.binds}

let get_bind scp str = List.assoc str scp

let paren x = "\n(" ^ x ^ ")"
let brack x = " [" ^ x ^ "] "
let brace x = " {" ^ x ^ "} "

let rec join_on s xs =
  match xs with
  | [] -> ""
  | [x] -> x
  | x :: xs -> x ^ s ^ join_on s xs

let (<|) f x = f x

(*



  the grammar of this is basically

  toplevel: (extern | assign)*

  extern: (extern {typ} id)

  assign: (tlet {typ} id (args) (expr))

  expr:
  | ident: (bound {typ} id)
  | val: (int/float/string val)
  | tuple: (tuple (expr)* )
  | bools
  | fcall: (fcall (expr) (expr))
  | let: (let {typ} id (e1) (e2))
  | tupacc: (tupaccess int (e))
  | lam: (lam {typ} id (e))
  | typlam: (typelam t (e))
  | seq: (seq {typ} (expr)* )
  | ifelse: (ifelse {typ} (cond) (e1) (e2))
*)


let rec codegen_base b scope =
  match b with
  | Ident(inf, t) ->
     "bound " ^ brace (gst inf) ^
       begin
         try
           List.assoc t scope.binds
         with
         | Not_found -> mangle t
       end |> paren
  | Int(s) -> "int " ^ s |> paren
  | Float(s) -> "float "  ^ s |> paren
  | Str(s) -> "string \"" ^ s ^ "\"" |> paren
  | Tuple(ex) -> "tuple "
                 ^ join_on " "
                     (List.map (fun x -> codegen_expr x scope) ex)
                 |> paren
  | True -> "bool true" |> paren
  | False -> "bool false" |> paren

and codegen_expr ex scope =
  match ex with
  | Base(_, k) -> codegen_base k scope
  | FCall(_, f, x) -> "fcall " ^ codegen_expr f scope
                   ^ codegen_expr x scope |> paren
  | AnnotLet(inf, l, _, e1, e2)
    | LetIn(inf, l, e1, e2) -> "let " ^ brace (gst inf) ^ mangle l
                          ^ codegen_expr e1 scope
                          ^ codegen_expr e2 scope |> paren
  | Join(inf, e1, e2) -> "seq " ^ brace (gst inf) ^ codegen_expr e1 scope
                    ^ codegen_expr e2 scope |> paren
  | Inst(_, _, _) -> raise <| Impossible "INST"
  | IfElse(inf, c, e1, e2) -> "ifelse " ^ brace (gst inf)
                         ^ codegen_expr c scope ^ codegen_expr e1 scope
                         ^ codegen_expr e2 scope |> paren
  | Lam(inf, l, e)
    | AnnotLam(inf, l, _, e) -> "lam " ^ brace (gst inf) ^ mangle l
                           ^ codegen_expr e scope |> paren
  | TupAccess(_, e, i) -> "tupaccess " ^ string_of_int i
                       ^ codegen_expr e scope |> paren
  | TypeLam(_, t, e) -> "typelam " ^ t ^ codegen_expr e scope |> paren
    
let codegen_assign a e scope =
  let (id, ts) = a in
  let (_, args, body) = e in
  let body' = conv_ts_args_body_to_typelams ts args body in
  begin
    "tlet "
    ^ brace (pshow_typesig ts)
    ^ mangle id ^ " "
    ^ paren (join_on " " (List.map mangle args))
    ^ paren (codegen_expr body' scope)
    |> paren
  end
  ^ "\n"

let rec codegen_program p s =
  let scp = match s with
    | Some(x) -> x
    | None -> new_scope ()
  in 
  match p with
  | Program([]) -> ""
  | Program(x :: xs) ->
     let scp'str = match x with
       | TopAssign(a, e) -> (add_bind scp (fst a) (mangle (fst a)),
                             codegen_assign a e scp)
       | Extern(id, ts) -> (add_bind scp id id, "\n(extern " ^ brace (pshow_typesig ts) ^ id ^ ")\n")
     in
     snd scp'str ^ codegen_program (Program(xs)) (Some(fst scp'str))
     

let rec codegen names programs =
  match programs with
  | [] -> ""
  | x :: xs -> codegen_program x None ^ codegen names xs
