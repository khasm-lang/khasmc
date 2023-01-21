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
  if str = "()" then "Khasmc_int.bottom" else
    "Khasmc." ^ str


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

let rec dup s ind =
  match ind with
  | 0 -> ""
  | x -> s ^ dup s (x - 1)

let gen ind x =
  if String.starts_with ~prefix:"\n" x then
    "\n" ^ dup "  " ind ^ String.sub x 1 (String.length x - 1)
  else
    dup "  " ind ^ x

(*
  the grammar of this is basically

  toplevel: (extern | assign)*

  extern: (extern  id)

  assign: (tlet  id (args) (expr))

  expr:
  | ident: (bound  id)
  | val: (int/float/string val)
  | tuple: (tuple (expr)* )
  | bools
  | fcall: (fcall (expr) (expr))
  | let: (let  id (e1) (e2))
  | tupacc: (tupaccess int (e))
  | lam: (lam  id (e))
  | typlam: (typelam t (e))
  | seq: (seq  (expr)* )
  | ifelse: (ifelse  (cond) (e1) (e2))
*)


(*
  The decl of the pprinter for types is in ast.ml, because
  ocaml doesn't support recursive modules :(
 *)

let rec codegen_base b scope ind =
  match b with
  | Ident(_, t) ->
     "bound " ^  
       begin
         try
           List.assoc t scope.binds
         with
         | Not_found -> mangle t
       end |> paren |> gen ind
  | Int(s) -> "int " ^ s |> paren |> gen ind
  | Float(s) -> "float "  ^ s |> paren |> gen ind
  | Str(s) -> "string \"" ^ s ^ "\"" |> paren |> gen ind
  | Tuple(ex) -> "tuple "
                 ^ join_on " "
                     (List.map (fun x -> codegen_expr x scope (ind + 1)) ex)
                 |> paren |> gen ind
  | True -> "bool true" |> paren |> gen ind
  | False -> "bool false" |> paren |> gen ind



and codegen_expr ex scope ind =
  match ex with
  | Base(_, k) -> codegen_base k scope (ind)
  | FCall(_, f, x) -> "fcall "
                      ^ codegen_expr f scope (ind + 1)
                      ^ codegen_expr x scope (ind + 1)
                      |> paren |> gen ind
  | AnnotLet(_, l, _, e1, e2)
    | LetIn(_, l, e1, e2) -> "let "
                             ^   mangle l
                             ^ codegen_expr e1 scope(ind + 1)
                             ^ codegen_expr e2 scope (ind + 1)
                             |> paren |> gen ind

  | Join(_, e1, e2) -> "seq "
                       ^ codegen_expr e1 scope(ind + 1)
                       ^ codegen_expr e2 scope (ind + 1)
                       |> paren |> gen ind

  | Inst(_, _, _) -> raise <| Impossible "INST"
  | IfElse(_, c, e1, e2) -> "ifelse "
                            ^ codegen_expr c scope (ind + 1)
                            ^ codegen_expr e1 scope(ind + 1)
                            ^ codegen_expr e2 scope (ind + 1)
                            |> paren |> gen ind
  | Lam(_, l, e)
    | AnnotLam(_, l, _, e) -> "lam "
                              ^ mangle l
                              ^ codegen_expr e scope (ind + 1)
                              |> paren |> gen ind
  | TupAccess(_, e, i) -> "tupaccess "
                          ^ string_of_int i
                          ^ codegen_expr e scope (ind + 1)
                          |> paren |> gen ind

  | TypeLam(_, _, e) -> codegen_expr e scope ind
    
let codegen_assign a e scope =
  let (id, _) = a in
  let (_, args, body) = e in
  let body' = body in
  begin
    "tlet "
    ^ mangle id ^ " "
    ^ paren (join_on " " (List.map mangle args))
    ^ (codegen_expr body' scope 0)
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
       | Extern(id, _) -> (add_bind scp id id, "\n(extern " ^ id ^ ")\n")
     in
     snd scp'str ^ codegen_program (Program(xs)) (Some(fst scp'str))
     

let rec codegen names programs =
  match programs with
  | [] -> ""
  | x :: xs -> codegen_program x None ^ codegen names xs
