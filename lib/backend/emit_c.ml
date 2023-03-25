open Exp
open Format
open Khagm
open KhasmUTF

let mangler id =
  let asint = String.get_uint8 id 0 in
  if asint >= 65 && asint < 65 + 26 then id
  else if asint >= 97 && asint < 97 + 26 then id
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

let mangle_top_str id = "extern_" ^ utf8_map mangler id

let mangle id =
  let id' = string_of_int id in
  "khasm_" ^ id'

let gen i = String.concat "" @@ List.init i (fun _ -> " ")

let rec compute_predecls exp =
  match exp with
  | Tuple t -> List.map compute_predecls t |> List.flatten
  | Seq (e1, e2) | Call (e1, e2) -> compute_predecls e1 @ compute_predecls e2
  | Let (v, e1, e2) -> (mangle v :: compute_predecls e1) @ compute_predecls e2
  | IfElse (c, e1, e2) ->
      compute_predecls e1 @ compute_predecls e2 @ compute_predecls c
  | Val _ | Unboxed _ -> []

let rec emit_unboxed u =
  let f =
    match u with
    | Float' s -> "create_float(" ^ s ^ ")"
    | String' s -> "create_string(\"" ^ String.escaped s ^ "\")"
    | Int' s -> "create_int(" ^ s ^ ")"
    | Bool' b -> (
        match b with true -> "create_int(1)" | false -> "create_int(0)")
  in
  f ^ "\n"

let addr nms id =
  match List.assoc_opt id nms with
  | Some _ -> "create_call(&" ^ mangle_top nms id ^ ", NULL, 0)"
  | None -> mangle id

let create_maybe_call id nms list =
  match List.assoc_opt id nms with
  | Some _ -> "create_call(&" ^ mangle_top nms id ^ "," ^ list ^ ", 1)"
  | None -> "create_thunk(" ^ mangle id ^ ",\n " ^ list ^ ", 1)"

let gen_funcsig id nms args =
  "khagm_obj * " ^ mangle_top nms id ^ "("
  ^ (List.map (( ^ ) "khagm_obj * ") (List.map mangle args)
    |> String.concat ", ")
  ^ ") {\n"

let gen_predecls predecls =
  List.map (( ^ ) "khagm_obj * ") predecls
  |> List.map (fun x -> x ^ ";\n")
  |> String.concat " "

let rec emit_expr nms exp =
  let first =
    match exp with
    | Val s -> addr nms s
    | Unboxed u -> emit_unboxed u
    | Tuple t -> (
        let len = List.length t |> string_of_int in
        match len with
        | "0" -> "create_tuple(NULL, 0)"
        | _ ->
            let str = List.map (emit_expr nms) t |> String.concat ", " in
            let list = "create_list(" ^ len ^ ", " ^ str ^ ")" in
            "create_tuple(" ^ list ^ ", " ^ len ^ ")")
    | Call (Val id, e2) ->
        let e2' = emit_expr nms e2 in
        let list = "create_list(1,\n (" ^ e2' ^ "))" in
        create_maybe_call id nms list
    | Call (e1, e2) ->
        let e1' = emit_expr nms e1 in
        let e2' = emit_expr nms e2 in
        let list = "create_list(1,\n (" ^ e2' ^ "))" in
        "create_thunk(" ^ e1' ^ ", " ^ list ^ ", 1)"
    | Seq (e1, e2) ->
        let e1' = emit_expr nms e1 in
        let e2' = emit_expr nms e2 in
        "create_seq(" ^ e1' ^ ",\n " ^ e2' ^ ")"
    | Let (v, e1, e2) ->
        let e1' = emit_expr nms e1 in
        let e2' = emit_expr nms e2 in
        "((" ^ mangle v ^ " = " ^ e1' ^ "),\n (" ^ e2' ^ "))"
    | IfElse (c, e1, e2) ->
        let c' = emit_expr nms c in
        let e1' = emit_expr nms e1 in
        let e2' = emit_expr nms e2 in
        let list =
          "create_list(3,\n " ^ String.concat ",\n " [ c'; e1'; e2' ] ^ ")"
        in
        "create_ITE(" ^ list ^ ")"
  in
  first ^ "\n"

let gen_return code = "\nreturn " ^ code ^ ";\n"

let rec emit_top nms code =
  match code with
  | Let (id, args, exp) ->
      let code = emit_expr nms exp in
      let pres = compute_predecls exp in
      let sig' = gen_funcsig id nms args in
      let predecls = gen_predecls pres in
      let code =
        sig' ^ predecls ^ gen_return code ^ "}"
        |> ( ^ ) ("\n/* " ^ List.assoc id nms ^ " */\n")
      in
      (code, Some (mangle_top nms id, List.length args, mangle_top nms id))
  | Extern (id, arity, str) ->
      ( "#define " ^ mangle_top nms id ^ " " ^ mangle_top_str str,
        Some (mangle_top_str str, arity, str) )

let top_prelude =
  {|
  /* Compiler generated khasm code,
    running on the Khagm graph backend. */
|}

let t_fst (a, _, _) = a
let t_snd (_, b, _) = b
let t_thrd (_, _, c) = c

let rec arity_h list =
  match list with
  | [] -> ""
  | Some x :: xs ->
      "if (f == &" ^ t_fst x ^ ") return "
      ^ string_of_int (t_snd x)
      ^ ";\n" ^ arity_h xs
  | None :: xs -> arity_h xs

let gen_arity_table list =
  "int arity_table(fptr f) {\n" ^ arity_h list ^ "return -1;\n}"

let rec getval_h list =
  match list with
  | [] -> ""
  | Some x :: xs ->
      "if (f == &" ^ t_fst x ^ ") return " ^ "\"" ^ t_thrd x ^ "\"" ^ ";\n"
      ^ getval_h xs
  | None :: xs -> getval_h xs

let gen_get_val list =
  "char * get_val_from_pointer(fptr f){\n" ^ getval_h list
  ^ "; return \"NO NAME\";\n}"

let emit (prog : khagm) =
  let code, nms = prog in
  let res = List.map (emit_top nms) code in
  let code = List.map fst res in
  let aritytable = gen_arity_table (List.map snd res) in
  let valtable = gen_get_val (List.map snd res) in
  let b = String.concat "\n/* -------- */\n" code |> ( ^ ) top_prelude in
  b ^ aritytable ^ valtable