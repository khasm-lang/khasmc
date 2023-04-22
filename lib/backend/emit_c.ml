open Exp
open Format
open Khagm
open Add_new
open KhasmUTF

(* TODO: add compiler generated names to nms dict *)

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

let mangle_top_str id = "extern_" ^ utf8_map mangler id

let mangle id =
  let id' = string_of_int id in
  "khasm_" ^ id'

let gen i = String.concat "" @@ List.init i (fun _ -> " ")
let predecl s = mangle s ^ " = k_alloc(sizeof(khagm_obj));\n"

let rec compute_predecls exp =
  match exp with
  | Tuple t -> List.map compute_predecls t |> List.flatten
  | Seq (e1, e2) | Call (e1, e2) -> compute_predecls e1 @ compute_predecls e2
  | Let (v, e1, e2) -> (predecl v :: compute_predecls e1) @ compute_predecls e2
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
  | Some _ -> "create_call(&" ^ mangle_top nms id ^ ", create_list(0, NULL), 0)"
  | None -> mangle id

let create_maybe_call id nms list =
  match List.assoc_opt id nms with
  | Some _ -> "create_call(&" ^ mangle_top nms id ^ "," ^ list ^ ", 1)"
  | None -> "create_thunk(" ^ mangle id ^ ",\n " ^ list ^ ", 1)"

let gen_funcsig id nms args =
  "khagm_obj * " ^ mangle_top nms id ^ "("
  ^ "khagm_obj ** khagm__args, i32 khagm__argnum) {\n" ^ "if (khagm__argnum < "
  ^ (string_of_int @@ List.length args)
  ^ ") {return NULL;}\n" ^ "khagm_obj "
  ^ String.concat ", "
      (List.mapi
         (fun i a -> "*" ^ mangle a ^ " = khagm__args[" ^ string_of_int i ^ "]")
         args)
  ^ ";\n"

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
        "((memmove(" ^ mangle v ^ ", " ^ e1' ^ ", sizeof(khagm_obj))),\n ("
        ^ e2' ^ "))"
    | IfElse (c, e1, e2) ->
        let c' = emit_expr nms c in
        let e1' = emit_expr nms e1 in
        let e2' = emit_expr nms e2 in
        "(khagm_whnf(" ^ c' ^ "))->data.unboxed_int ? (" ^ e1' ^ ") : (" ^ e2'
        ^ ")"
  in
  first ^ "\n"

let gen_return code args =
  "\nreturn set_used(" ^ code ^ ", " ^ string_of_int (List.length args) ^ ");\n"

let rec emit_top nms code =
  match code with
  | Let (id, args, exp) ->
      let code = emit_expr nms exp in
      let pres = compute_predecls exp in
      let sig' = gen_funcsig id nms args in
      let predecls = gen_predecls pres in
      let code =
        sig' ^ predecls ^ gen_return code args ^ "}"
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

let rec gen_toplevel_predecls nms =
  match nms with
  | [] -> ""
  | x :: xs ->
      let x' = fst x in
      "khagm_obj * " ^ mangle_top [ x ] x' ^ "(khagm_obj **, i32);"
      ^ gen_toplevel_predecls xs

let emit (prog : khagm) =
  let code, nms = prog in
  let nms = add_new code nms in
  let res = List.map (emit_top nms) code in
  let code = List.map fst res in
  let b = String.concat "\n/* -------- */\n" code |> ( ^ ) top_prelude in
  gen_toplevel_predecls nms ^ b
