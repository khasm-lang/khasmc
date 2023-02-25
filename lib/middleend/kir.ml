open Exp

type kirtype = Ast.typesig [@@deriving show { with_path = false }]
type kirval = int [@@deriving show { with_path = false }]
type transtable = (int * string) list [@@deriving show { with_path = false }]

let empty_transtable () = (-1, "") :: []
let rint = ref 1

let get_random_num () =
  let tmp = !rint in
  rint := !rint + 1;
  tmp

let add_to_tbl str tbl =
  let random = if str = "main" then 0 else get_random_num () in
  let tbl' = (random, str) :: tbl in
  (random, tbl')

let get_from_tbl str tbl =
  match List.find_opt (fun x -> snd x = str) tbl with
  | Some s -> s
  | None -> raise @@ NotFound (str ^ " not found in table")

let add_alias_to_tbl str1 str2 tbl =
  let id, _ = get_from_tbl str2 tbl in
  (id, (id, str1) :: tbl)

type kirexpr =
  | Val of kirtype * kirval
  | Int of string
  | Float of string
  | Str of string
  | Bool of bool
  | Tuple of kirtype * kirexpr list
  | Call of kirtype * kirexpr * kirexpr
  | Seq of kirtype * kirexpr * kirexpr
  | TupAcc of kirtype * kirexpr * int
  | Lam of kirtype * kirval * kirexpr
  | Let of kirtype * kirval * kirexpr * kirexpr
  | IfElse of kirtype * kirexpr * kirexpr * kirexpr
[@@deriving show { with_path = false }]

type kirtop =
  | Let of kirtype * kirval * kirexpr
  | LetRec of kirtype * kirval * kirexpr
  | Extern of kirtype * kirval * string
  | Bind of kirval * kirval
[@@deriving show { with_path = false }]

type kirprog = transtable * kirtop list [@@deriving show { with_path = false }]

let rec kirexpr_typ k =
  match k with
  | Val (t, _) -> t
  | Int _ -> Ast.TSBase "int"
  | Float _ -> Ast.TSBase "float"
  | Str _ -> Ast.TSBase "string"
  | Bool _ -> Ast.TSBase "bool"
  | Tuple (t, _) -> t
  | Call (t, _, _) -> t
  | Seq (t, _, _) -> t
  | TupAcc (t, _, _) -> t
  | Lam (t, _, _) -> t
  | Let (t, _, _, _) -> t
  | IfElse (t, _, _, _) -> t
