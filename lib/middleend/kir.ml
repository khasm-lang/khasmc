open Exp

(* The middleend IR *)

type kirtype = Ast.typesig [@@deriving show { with_path = false }]
type kirval = int [@@deriving show { with_path = false }]

type kir_table = {
  binds : (kirval * string) list; (*  name   arity  id within respective type *)
  constrs : (string * int * kirval) list;
}
[@@deriving show { with_path = false }]

let add_bind tbl id v = { tbl with binds = (v, id) :: tbl.binds }
let get_bind tbl v = List.find_opt (fun (_, b) -> b = v) tbl.binds
let get_bind_id tbl v = List.find_opt (fun (b, _) -> b = v) tbl.binds

let add_constr tbl id arity num =
  { tbl with constrs = (id, arity, num) :: tbl.constrs }

let get_constr tbl id = List.find_opt (fun (a, _, _) -> a = id) tbl.constrs

let is_constr tbl nm =
  match List.filter (fun (b, _, _) -> b = nm) tbl.constrs with
  | _ :: _ -> true
  | [] -> false

let empty_transtable () = { binds = [ (-1, ""); (-2, "()") ]; constrs = [] }
let rint = ref 1

let get_random_num () =
  let tmp = !rint in
  rint := !rint + 1;
  tmp

let add_to_tbl str tbl =
  let random =
    if str = "main" then
      0
    else
      get_random_num ()
  in
  let tbl' = add_bind tbl str random in
  (random, tbl')

let get_from_tbl str tbl =
  match get_bind tbl str with
  | Some a -> a
  | None -> raise @@ NotFound (str ^ " not found in table")

let add_alias_to_tbl str1 str2 tbl =
  let id, _ = get_from_tbl str2 tbl in
  (id, add_bind tbl str1 id)

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
  | SwitchConstr of kirtype * kirexpr * (matchbind * kirexpr) list
[@@deriving show { with_path = false }]

and matchbind =
  (* arity * constr number * bindto *)
  | BindN of int * int * kirval list
  (* arity * bindto *)
  | BindTup of int * kirval list
[@@deriving show { with_path = false }]

type kirtop =
  | Let of kirtype * kirval * kirexpr
  | LetRec of kirtype * kirval * kirexpr
  | Extern of kirtype * int * kirval * string
  | Bind of kirval * kirval
  | Noop
[@@deriving show { with_path = false }]

type kirprog = kir_table * kirtop list [@@deriving show { with_path = false }]

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
