type fident =
  | Bot of string
  | Mod of string * fident (* mod.x *)
  | Struc of string * fident (* struc:x *)
[@@deriving show { with_path = false }, eq]

let rec unqual x =
  match x with Bot y -> y | Mod (_, y) -> unqual y | Struc (_, y) -> unqual y

exception Impossible of string

let fullident_ensure_str x =
  match x with
  | Str.Text y -> y
  | Str.Delim _ -> raise (Impossible "fullident_ensure_str")

let fullident_ensure_delim x =
  match x with
  | Str.Text _ -> raise (Impossible "fullident_ensure_delim")
  | Str.Delim y -> y

let rec build_fullident x =
  match x with
  | [] -> raise (Impossible "build_fullident 1")
  | [ y ] -> Bot (fullident_ensure_str y)
  | y1 :: y2 :: ys -> (
      match fullident_ensure_delim y2 with
      | ":" -> Struc (fullident_ensure_str y1, build_fullident ys)
      | "." -> Mod (fullident_ensure_str y1, build_fullident ys)
      | _ -> raise (Impossible "build_fullident 2"))

let process_fullident s =
  let reg = Str.regexp "([:] | [.])" in
  let whole = Str.full_split reg s in
  build_fullident whole

let rec mod_from_list l e =
  match l with
  | [ x ] -> Mod (x, Bot e)
  | x :: xs -> Mod (x, mod_from_list xs e)
  | [] -> Bot e

let rec str_of_fident f =
  match f with
  | Bot x -> x
  | Mod (x, y) -> x ^ "." ^ str_of_fident y
  | Struc (x, y) -> x ^ ":" ^ str_of_fident y
