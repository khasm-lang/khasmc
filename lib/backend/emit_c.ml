open Exp
open Format
open Khagm

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
  match u with
  | Float' s -> "create_float(" ^ s ^ ")"
  | String' s -> "create_string(\"" ^ s ^ "\")"
  | Int' s -> "create_int(" ^ s ^ ")"
  | Bool' b -> (
      match b with true -> "create_int(1)" | false -> "create_int(0)")

let addr nms id =
  match List.assoc_opt id nms with
  | Some _ -> "create_call(&" ^ mangle id ^ ", NULL, 0)"
  | None -> mangle id

let gen_funcsig id args =
  "khagm_obj * " ^ mangle id ^ "("
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
        let nm = addr nms id in
        let list = "create_list(1, " ^ e2' ^ ")" in
        "create_call(" ^ nm ^ ", " ^ list ^ ", 1)"
    | Call (e1, e2) ->
        let e1' = emit_expr nms e1 in
        let e2' = emit_expr nms e2 in
        let list = "create_list(1, " ^ e2' ^ ")" in
        "create_thunk(" ^ e1' ^ ", " ^ list ^ ", 1)"
    | Seq (e1, e2) ->
        let e1' = emit_expr nms e1 in
        let e2' = emit_expr nms e2 in
        "((" ^ e1' ^ "), (" ^ e2' ^ "))"
    | Let (v, e1, e2) ->
        let e1' = emit_expr nms e1 in
        let e2' = emit_expr nms e2 in
        "((" ^ mangle v ^ " = " ^ e1' ^ "), (" ^ e2' ^ "))"
    | IfElse (c, e1, e2) ->
        let c' = emit_expr nms c in
        let e1' = emit_expr nms e1 in
        let e2' = emit_expr nms e2 in
        let list =
          "create_list(3, " ^ String.concat ", " [ c'; e1'; e2' ] ^ ")"
        in
        "create_ITE(" ^ list ^ ")"
  in
  first

let rec emit_top nms code =
  match code with
  | Let (id, args, exp) ->
      let code = emit_expr nms exp in
      let pres = compute_predecls exp in
      let sig' = gen_funcsig id args in
      let predecls = gen_predecls pres in
      sig' ^ predecls ^ "return " ^ code ^ ";\n}\n"
  | Extern (id, str) -> "\n/* TODO: externs */\n"

let emit (prog : khagm) =
  let code, nms = prog in
  let res = List.map (emit_top nms) code in
  String.concat "\n/* -------- */\n" res
