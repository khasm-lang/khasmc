type kirtype = Ast.typesig [@@deriving show { with_path = false }]
type kirval = int [@@deriving show { with_path = false }]
type transtable = (int, string) Hashtbl.t ref

let pp_transtable runtime table =
  Hashtbl.iter
    (fun x y ->
      Ppx_deriving_runtime.Format.fprintf runtime "%s: " (string_of_int x);
      Ppx_deriving_runtime.Format.fprintf runtime "%s\n" y)
    !table

let empty_transtable () = ref @@ Hashtbl.create ~random:true 10
let rint = ref 0

let get_random_num () =
  let tmp = !rint in
  rint := !rint + 1;
  tmp

let add_to_tbl id tbl =
  let random = get_random_num () in
  Hashtbl.add !tbl random id;
  random

let get_from_tbl str tbl =
  match Hashtbl.to_seq !tbl |> Seq.find (fun x -> snd x = str) with
  | Some (a, _) -> Some a
  | None -> None

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
  | Lam of kirtype * kirexpr
  | IfElse of kirtype * kirexpr * kirexpr * kirexpr
[@@deriving show { with_path = false }]

type kirtop =
  | Let of kirtype * kirval * kirexpr
  | LetRec of kirtype * kirval * kirexpr
  | Extern of kirtype * kirval
[@@deriving show { with_path = false }]

type kirprog = transtable * kirtop list [@@deriving show { with_path = false }]
