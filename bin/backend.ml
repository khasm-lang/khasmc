open Hash
open Ast
open Exp



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
  chars_of_string str
  |> List.map Char.code
  |> List.map mangle_c
  |> String.concat ""
  |> (^) "Khasmc_"


type scope = {
    binds: (string * string) list;
  }

let new_scope () =
  {binds = ("()", "()") :: []}

let add_bind scp id bd =
  {binds = (id, bd) :: scp.binds}

let get_bind scp str = List.assoc str scp

let codegen_assign a e scope =
  let (id, ts) = a in
  let (_, args, body) = e in
  "STUB"

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
       | Extern(id, ts) -> (add_bind scp id id, "\n(extern " ^ id ^ ")\n")
     in
     snd scp'str ^ codegen_program (Program(xs)) (Some(fst scp'str))
     
