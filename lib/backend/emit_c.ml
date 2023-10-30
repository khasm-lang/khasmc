open Exp
open Khagm
open KhasmUTF
open ListHelpers

open Count
(** Emit the C equivalent of the program *)

type info = {
  arties : (id * int) list;
  externs : id list;
}

let mangler id =
  let asint = String.get_uint8 id 0 in
  if asint >= 65 && asint < 65 + 26 then
    id
  else if asint >= 97 && asint < 97 + 26 then
    id
  else if asint >= 48 && asint < 48 + 10 then
    id
  else
    "_" ^ string_of_int asint ^ "_"

let quot_lam id = utf8_map mangler ("khasm_lam_" ^ string_of_int id)

let mangle tbl id =
  match Kir.get_bind_id tbl id with
  | None -> quot_lam id
  | Some (_, nm) -> (
      match nm with "main" -> "main_____Khasm" | _ -> utf8_map mangler nm)

let mangle_extern name =
  utf8_map
    (fun x ->
      if x = "`" then
        ""
      else
        x)
    name

let id_to_name id =
  if id >= 0 then
    let id' = string_of_int id in
    "khasm_" ^ id'
  else
    let id' = string_of_int (-id) in
    "khasm_neg_" ^ id'

let is_extern ctx id = List.mem id ctx.externs

let quot ctx tbl id =
  match Kir.get_bind_id tbl id with
  | None -> id_to_name id
  | Some _ -> "make_raw_ptr(&" ^ mangle tbl id ^ ")"

let quot_raw _ctx tbl id =
  match Kir.get_bind_id tbl id with
  | None -> "(" ^ id_to_name id ^ ")"
  | Some _ -> "make_raw_ptr(&" ^ mangle tbl id ^ ")"

let quot_dircall _ctx tbl id =
  match Kir.get_bind_id tbl id with
  | None -> quot_lam id
  | Some _ -> mangle tbl id

let rec quotval ctx tbl value =
  match value with
  | Val i -> quot ctx tbl i
  | Int i -> "make_int(" ^ i ^ ")"
  | String s -> "make_string(" ^ String.escaped s ^ ")"
  | Float s -> "make_float(" ^ s ^ ")"
  | Bool s ->
      "make_int("
      ^ (if s = "true" then
           "1"
         else
           "0")
      ^ ")"
  | Tuple vl ->
      "make_tuple("
      ^ string_of_int (List.length vl)
      ^ (if List.length vl = 0 then
           ""
         else
           ", ")
      ^ String.concat ", " (List.map (quotval ctx tbl) vl)
      ^ ")"

let comsep ctx tbl xs =
  ListHelpers.map (quot_raw ctx tbl) xs |> String.concat ", "

let rec gen_funccall ctx _ret tbl arties id func args =
  match
    (is_extern ctx id, Kir.get_bind_id tbl func, List.assoc_opt func arties)
  with
  (*| false, Some _, Some arity when arity = List.length args ->
      let funcname = quot_dircall ctx tbl func in
      quot ctx tbl id ^ " = " ^ funcname ^ "(" ^ comsep ctx tbl args ^ ");\n" *)
  | _, _, _ ->
      let rec go call args =
        match args with
        | [] -> quot ctx tbl id ^ " = (" ^ quot ctx tbl call ^ ");\n"
        | x :: xs ->
            let tmp = unique () in
            let curr =
              quot ctx tbl tmp
              ^ " = "
              ^ "add_arg("
              ^ quot_raw ctx tbl call
              ^ ", "
              ^ quot ctx tbl x
              ^ ");\n"
            in
            let next = go tmp xs in
            curr ^ next ^ quot ctx tbl tmp ^ ";"
      in
      let tmp = go func args in
      tmp

and emit_body ctx ret tbl arties body =
  match body with
  | Fail s ->
      {|fprintf(stderr, "FAILURE: |}
      ^ s
      ^ {| LINE: %d FILE: %s\n", __LINE__, __FILE__);|}
      ^ " exit(1);\n"
  | Return i -> quot ctx tbl ret ^ " = " ^ quot ctx tbl i ^ ";\n"
  | LetInVal (id, value) ->
      quot ctx tbl id ^ " = " ^ quotval ctx tbl value ^ ";\n"
  | LetInCall (id, func, args) ->
      if List.length args = 0 then
        quot ctx tbl id ^ " = " ^ quot ctx tbl func ^ ";\n"
      else
        gen_funccall ctx ret tbl arties id func args
  | LetInUnboxCall (id, func, args) ->
      quot ctx tbl id
      ^ " = "
      ^ quot_dircall ctx tbl func
      ^ "("
      ^ String.concat ", " (List.map (fun x -> "(" ^ quot ctx tbl x ^ ")") args)
      ^ ");\n"
  | Special (id, value, spec) -> (
      match spec with
      | TupAcc i ->
          quot ctx tbl id
          ^ " = "
          ^ "khasm_tuple_acc("
          ^ quotval ctx tbl (Int (string_of_int i))
          ^ ", "
          ^ quotval ctx tbl value
          ^ ");\n")
  | SubExpr (id, exprs) ->
      let sub = ListHelpers.map (emit_body ctx id tbl arties) exprs in
      "{\n" ^ String.concat " " sub ^ "\n}\n"
  | CheckCtor (id, value, ctor) ->
      quot ctx tbl id
      ^ " = make_int("
      ^ quot ctx tbl value
      ^ "->data.adt.tag == "
      ^ string_of_int ctor
      ^ ");\n"
  | IfElse (id, cond, e1, e2) ->
      let e1' = ListHelpers.map (emit_body ctx id tbl arties) e1 in
      let e2' = ListHelpers.map (emit_body ctx id tbl arties) e2 in
      "if((u64)"
      ^ quot ctx tbl cond
      ^ " >> 1)"
      ^ "{\n"
      ^ String.concat " " e1'
      ^ "}\n else {\n"
      ^ String.concat " " e2'
      ^ "}\n"

let genfunc tbl id args =
  let name = mangle tbl id in
  let args = ListHelpers.map id_to_name args in
  let argscom =
    String.concat ", " @@ ListHelpers.map (( ^ ) "kha_obj * ") args
  in
  let extern = "extern kha_obj * " ^ name ^ "(" ^ argscom ^ ");" in
  let entry =
    "KHASM_ENTRY("
    ^ name
    ^ ","
    ^ (string_of_int @@ List.length args)
    ^ ", "
    ^ argscom
    ^ ")"
  in
  fun body -> extern ^ "\n" ^ entry ^ "{\n" ^ body ^ "\n}"

let rec get_frees_h body =
  match body with
  | LetInVal (i, _) -> [ i ]
  | LetInCall (i, _, _) -> [ i ]
  | IfElse (i, _, e1, e2) ->
      (i :: List.concat_map get_frees_h e1) @ List.concat_map get_frees_h e2
  | SubExpr (i, e) -> i :: List.concat_map get_frees_h e
  | Special (i, _, _) -> [ i ]
  | LetInUnboxCall (i, _, _) -> [ i ]
  | CheckCtor (i, _, _) -> [ i ]
  | _ -> []

let get_frees ctx tbl body bef after =
  List.concat (List.map get_frees_h body) |> fun x ->
  ListHelpers.increasing bef (after - bef) @ x
  |> List.map (quot ctx tbl)
  |> List.map (fun x -> " *" ^ x)
  |> String.concat ", "
  |> ( ^ ) "kha_obj "
  |> fun x -> x ^ ";\n"

let emit_top ctx tbl arties code =
  match code with
  | Let (id, args, body) ->
      let ret = unique () in
      let decl = genfunc tbl id args in
      let body' =
        String.concat "\n"
        @@ ListHelpers.map (emit_body ctx ret tbl arties) body
      in
      let after = unique () in
      let frees = get_frees ctx tbl body ret after in
      decl @@ frees ^ body' ^ "\n return " ^ quot ctx tbl ret ^ ";\n"
  | Ctor (id, arity) ->
      let args = ListHelpers.for_n arity (fun _ -> unique ()) in
      let join c f = String.concat c (List.map f args) in
      let init =
        "KHASM_ENTRY("
        ^ quot_dircall ctx tbl id
        ^ ", "
        ^ string_of_int arity
        ^ ", "
        ^ join ", " (fun x -> "kha_obj * " ^ quot ctx tbl x)
        ^ ") "
      in
      let tmp = unique () in
      let qtmp = quot ctx tbl tmp in
      let (Some (id, nm)) = Kir.get_bind_id tbl id in
      let (Some (_, _, ctornum)) = Kir.get_constr tbl nm in
      let body =
        if arity = 0 then
          "kha_obj * "
          ^ qtmp
          ^ "= make_tuple(0);\n"
          ^ qtmp
          ^ "->data.adt.tag = "
          ^ string_of_int ctornum
          ^ ";\n"
          ^ qtmp
          ^ "->tag = ADT;\n"
          ^ "return "
          ^ qtmp
          ^ ";\n"
        else
          "kha_obj * "
          ^ qtmp
          ^ "= make_tuple("
          ^ string_of_int arity
          ^ ", "
          ^ join "," (quot ctx tbl)
          ^ ");\n"
          ^ qtmp
          ^ "->data.adt.tag = "
          ^ string_of_int ctornum
          ^ ";\n"
          ^ qtmp
          ^ "->tag = ADT;\n"
          ^ "return "
          ^ qtmp
          ^ ";\n"
      in
      init ^ "{\n" ^ body ^ "}\n"
  | Extern (id, _arity, name) ->
      let real = mangle_extern name in
      let fake = mangle tbl id in
      "#define " ^ fake ^ " " ^ real ^ "\n"
  | Noop -> "\n/* NOOP */\n"

let rec gen_arties = function
  | Let (id, args, _) :: xs -> (id, List.length args) :: gen_arties xs
  | Extern (id, arity, _) :: xs -> (id, arity) :: gen_arties xs
  | _ :: xs -> gen_arties xs
  | [] -> []

let rec gen_externs = function
  | Extern (id, _arity, _) :: xs -> id :: gen_externs xs
  | _ :: xs -> gen_externs xs
  | [] -> []

let emit_c khagm =
  let code, table = khagm in
  let arties = gen_arties code in
  let externs = gen_externs code in
  let ctx = { arties; externs } in
  let c = ListHelpers.map (emit_top ctx table arties) code in
  let final = String.concat "\n" c in
  final
