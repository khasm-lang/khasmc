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
  if id >= 0 then
    let id' = string_of_int id in
    "khasm_" ^ id'
  else
    let id' = string_of_int (-id) in
    "khasm_neg_" ^ id'

let function_name name = "\nkha_obj * " ^ name ^ "(u64 i, kha_obj **a) {"

let is_toplevel id tbl =
  match List.assoc_opt id tbl with Some _ -> true | None -> false

let lookup x (tbl : (khagmid * string) list) =
  match List.assoc_opt x tbl with
  | Some x -> x
  | None -> raise (Impossible "not_found")

let counter = ref 0

let gen_uniq_name () =
  let tmp = !counter in
  counter := !counter + 1;
  "tmp_var_" ^ string_of_int tmp

let emission = ref []
let adds_default () = [ "IFELSETEMP" ]
let adds = ref [ "IFELSETEMP" ]
let emit_ptr tbl name = "make_raw_ptr(&" ^ mangle_top tbl name ^ ")"

let emit_ref name = "ref(" ^ name ^ ")"

and gen_new s =
  let n' = gen_uniq_name () in
  emission := (n' ^ " = " ^ s ^ ";\n") :: !emission;
  adds := n' :: !adds;
  n'

let rec emit_tuple tbl x =
  let codes = List.map (fun x -> codegen_func x tbl) x in
  gen_new
    ("make_tuple("
    ^ (string_of_int @@ List.length x)
    ^ ", " ^ String.concat ", " codes ^ ")")

and emit_call tbl e1 e2 =
  let b1 = codegen_func e1 tbl in
  let b2 = codegen_func e2 tbl in
  gen_new ("call(" ^ b1 ^ ", " ^ b2 ^ ")")

and emit_unboxed b =
  match b with
  | Float' s -> "make_float(" ^ s ^ ")"
  | String' s -> "make_string(\"" ^ String.escaped s ^ "\")"
  | Int' s -> "make_int(" ^ s ^ ")"
  | Bool' b -> ( match b with true -> "make_int(1)" | false -> "make_int(0)")

and add_to_emi s = emission := s :: !emission

and codegen_func code tbl =
  match code with
  | Val v ->
      if is_toplevel v tbl then gen_new (emit_ptr tbl v)
      else gen_new (emit_ref (mangle v))
  | Unboxed v -> gen_new (emit_unboxed v)
  | Tuple t -> emit_tuple tbl t
  | Call (e1, e2) -> emit_call tbl e1 e2
  | Seq (e1, e2) ->
      ignore @@ codegen_func e1 tbl;
      codegen_func e2 tbl
  | Let (id, e1, e2) ->
      let n' = codegen_func e1 tbl in
      emission :=
        ("kha_obj * " ^ mangle id ^ " = ref(" ^ n' ^ ");\n") :: !emission;
      codegen_func e2 tbl
  | IfElse (c, e1, e2) ->
      let n1 = codegen_func c tbl in
      add_to_emi ("if (" ^ n1 ^ "->data.i) {\n");
      let t1 = codegen_func e1 tbl in
      if t1 = "IFELSETEMP" then ()
      else add_to_emi (";\n IFELSETEMP = ref(" ^ t1 ^ ");");
      add_to_emi "} else {";
      let t2 = codegen_func e2 tbl in
      if t2 = "IFELSETEMP" then ()
      else add_to_emi (";\n IFELSETEMP = ref(" ^ t2 ^ ");");
      add_to_emi "}\n";
      "IFELSETEMP"

let ensure_notempty args str = match args with [] -> "/*EMPTY*/" | _ -> str

let rec codegen code tbl =
  match code with
  | [] -> ""
  | x :: xs ->
      adds := adds_default ();
      emission := [];
      let part =
        match x with
        | Let (id, args, expr) ->
            let scaf = function_name (mangle_top tbl id) in
            let ensure_enough_args =
              "if (i < "
              ^ (string_of_int @@ List.length args)
              ^ ") {return NULL;}"
            in
            let set_used =
              "used = " ^ (string_of_int @@ List.length args) ^ ";\n"
            in
            let args_gen =
              ensure_notempty args @@ "kha_obj "
              ^ (String.concat ", "
                @@ List.mapi
                     (fun i x ->
                       "*" ^ mangle x ^ " = ref(a[" ^ string_of_int i ^ "])\n")
                     args)
              ^ ";\n"
            in
            (* STATEFUL *)
            let body = codegen_func expr tbl in
            let unrefs =
              (ensure_notempty args @@ String.concat " "
              @@ List.map (fun x -> "unref(" ^ mangle x ^ ");\n") args)
              ^ String.concat ""
                  (List.map (fun x -> "unref(" ^ x ^ ");\n") (List.rev !adds))
            in
            let adds' =
              ensure_notempty !adds @@ "kha_obj "
              ^ (String.concat ", "
                @@ List.map (fun x -> "*" ^ x ^ "= NULL") !adds)
              ^ ";\n"
            in
            scaf ^ ensure_enough_args ^ args_gen ^ adds'
            ^ String.concat "" (List.rev !emission)
            ^ "kha_obj * kha_return = ref(" ^ body ^ ");\n" ^ unrefs ^ set_used
            ^ "; return kha_return;}\n"
        | Extern (id, index, name) ->
            "/* EXTERN " ^ string_of_int id ^ " " ^ mangle index ^ " " ^ name
            ^ " */\n" ^ "extern kha_obj *" ^ mangle_top tbl id
            ^ "(u64, kha_obj **);"
      in
      part ^ codegen xs tbl

let prelude = {|
u64 used;
|}

let emit_c khagm =
  let code, tbl = khagm in
  let tbl = add_new code tbl in
  prelude ^ codegen code tbl
