open Khagm
open Add_new
open Exp
open KhasmUTF

let mangler id =
  let asint = String.get_uint8 id 0 in
  if asint >= 65 && asint < 65 + 26 then id
  else if asint >= 97 && asint < 97 + 26 then id
  else if asint >= 48 && asint < 48 + 10 then id
  else
    ((*these chars have to be invalid in identifiers
       - they serve as padding to make everything an i32*)
     (match unicode_len id with
     | 1 -> "???" ^ id
     | 2 -> "??" ^ id
     | 3 -> "?" ^ id
     | 4 -> id
     | _ -> raise @@ Impossible "unicode char len")
    |> (fun x -> String.get_int32_le x 0)
    |> Int32.to_string)
    ^ "_"

let mangle_top nms id =
  let nm = List.assoc id nms in
  match nm with
  | "main" -> "main_____Khasm"
  | _ -> "khasm_" ^ utf8_map mangler nm

let mangle id =
  let id' = string_of_int id in
  "khasm_" ^ id'

let function_name name = "\nkha_obj * " ^ name ^ "(int i, kha_obj **a)"

let is_toplevel id tbl =
  match List.assoc_opt id tbl with Some _ -> true | None -> false

let lookup x (tbl : (khagmid * string) list) =
  match List.assoc_opt x tbl with
  | Some x -> x
  | None -> raise (Impossible "not_found")

let emit_ptr tbl name = "make_ptr(&" ^ mangle_top tbl name ^ ")"
let emit_ref name = "ref(" ^ name ^ ")"

let rec emit_tuple x tbl =
  let tmp = List.map (fun x -> codegen_func x tbl) x in
  let codes = List.map fst tmp in
  let adds = List.flatten @@ List.map snd tmp in
  ( "make_tuple("
    ^ (string_of_int @@ List.length x)
    ^ ", " ^ String.concat ", " codes ^ ")",
    adds )

and emit_call e1 e2 tbl =
  let b1, add1 = codegen_func e1 tbl in
  let b2, add2 = codegen_func e2 tbl in
  ("call(" ^ b1 ^ ", " ^ b2 ^ ")", add1 @ add2)

and emit_unboxed b =
  match b with
  | Int' x -> "make_int(" ^ x ^ ")"
  | Float' x -> "make_float(" ^ x ^ ")"
  | Bool' x -> if x then "1" else "0"
  | String' x -> "make_string(\"" ^ Str.quote x ^ "\")"

and codegen_func code tbl =
  match code with
  | Val x ->
      if is_toplevel x tbl then (emit_ptr tbl x, [])
      else (emit_ref (mangle x), [])
  | Unboxed b -> (emit_unboxed b, [])
  | Tuple t -> emit_tuple t tbl
  | Call (e1, e2) -> emit_call e1 e2 tbl
  | Seq (e1, e2) ->
      let b1, add = codegen_func e1 tbl in
      let b2, add2 = codegen_func e2 tbl in
      ("(" ^ b1 ^ ", " ^ b2 ^ ")", add @ add2)
  | Let (id, e1, e2) ->
      let b1, add1 = codegen_func e1 tbl in
      let b2, add2 = codegen_func e2 tbl in
      ( "(" ^ mangle id ^ " = " ^ b1 ^ ", " ^ b2 ^ ")",
        mangle id :: (add1 @ add2) )
  | IfElse (c, e1, e2) ->
      let c', add1 = codegen_func c tbl in
      let b1, add2 = codegen_func e1 tbl in
      let b2, add3 = codegen_func e2 tbl in
      ("(" ^ c' ^ " ? " ^ b1 ^ " : " ^ b2 ^ ")", add1 @ add2 @ add3)

let rec codegen code tbl =
  match code with
  | [] -> ""
  | x :: xs ->
      let part =
        match x with
        | Let (id, args, expr) ->
            let scaf = function_name (mangle id) in
            let args_gen =
              "kha_obj "
              ^ (String.concat ", "
                @@ List.mapi
                     (fun i x ->
                       "*" ^ mangle x ^ " = a[" ^ string_of_int i ^ "]")
                     args)
              ^ ";"
            in
            let body, adds = codegen_func expr tbl in
            let adds =
              "kha_obj "
              ^ (String.concat ", " @@ List.map (fun x -> "*" ^ x) adds)
              ^ ";"
            in
            scaf ^ args_gen ^ adds ^ "return " ^ body ^ ";}\n"
        | Extern (id, index, name) ->
            "/* EXTERN " ^ string_of_int id ^ " " ^ mangle index ^ " " ^ name
            ^ " */\n"
      in
      part ^ codegen xs tbl

let emit_c khagm =
  let code, tbl = khagm in
  let tbl = add_new code tbl in
  codegen code tbl
