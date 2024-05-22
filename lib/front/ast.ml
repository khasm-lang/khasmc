(* The frontend result-of-parser AST. *)

type srcloc = {
  (* start * end *)
  row : int * int;
  col : int * int;
  slice : BatText.t;
}

type id = Common.Info.id [@@deriving show { with_path = false }]
type Common.Info.info += Srcloc
type Common.Info.data += Srcloc' of srcloc

type kind =
  (* type *)
  | Star
  (* type -> type *)
  | KArrow of kind * kind
[@@deriving show { with_path = false }]

type path =
  (* used to represent a path that doesn't lead anywhere *)
  | End
  (* x *)
  | Base of string
  (* Foo. *)
  | InMod of string * path
[@@deriving show { with_path = false }]

type pat =
  (* x -> ... *)
  | Bind of string
  (* (a, b) -> *)
  | PTuple of pat list
  (* Cons x xs -> *)
  | Constr of path * pat list
[@@deriving show { with_path = false }]

type ty =
  | TyString
  | TyInt
  | TyBool
  | TyChar
  (* 'a *)
  | Free of string
  (* MyInt *)
  | Custom of path
  (* (Int, Int) *)
  | Tuple of ty list
  (* a -> b *)
  | Arrow of ty * ty
  (* List Int *)
  | TApp of path * ty list
[@@deriving show { with_path = false }]

type tyexpr =
  (* variant name, list of types *)
  | TVariant of (string * ty list) list
  (* field name, ty *)
  | TRecord of (string * ty) list
[@@deriving show { with_path = false }]

type constraint' = path * ty list
[@@deriving show { with_path = false }]

let pp_exn fmt exn = Format.fprintf fmt "%s" (Printexc.to_string exn)

type tm =
  (* x *)
  | Var of id * string
  (* Foo.bar *)
  | Bound of id * path
  (* [f] a b c *)
  | App of id * tm * tm list
  (* let x = xs in h *)
  | Let of id * pat * tm * tm
  (* match f with pts...
     pat list to accomodate or patterns
  *)
  | Match of id * tm * (pat * tm) list
  (* fun x : t -> tm *)
  | Lam of id * pat * ty option * tm
  (* if x then y else z *)
  | ITE of id * tm * tm * tm
  (* x : t *)
  | Annot of id * tm * ty
  (* Foo {a = b; c = d}
     record fields must be strings
  *)
  | Record of id * path * (string * tm) list
  (* foo.bar *)
  | Project of id * path * string
  (* error *)
  | Poison of id * exn
[@@deriving show { with_path = false }]

type definition = {
  name : path;
  free_vars : string list;
  constraints : constraint' list;
  args : (string * ty) list;
  ret : ty;
  body : tm;
}
[@@deriving show { with_path = false }]

type trait = {
  name : path;
  args : (string * kind) list;
  (* any associated types *)
  assoc_types : (string * kind) list;
  (* any constraints on said & on inputs *)
  constraints : constraint' list;
  (* member functions *)
  functions : (string * string list * constraint' list * ty) list;
}
[@@deriving show { with_path = false }]

type impl = {
  name : path;
  args : ty list;
  assoc_types : (string * ty) list;
  impls : definition list;
}
[@@deriving show { with_path = false }]

type statement =
  (* name, freevars, constraints, args, return type, term *)
  | Definition of id * definition
  (* name, args, body *)
  | Type of id * path * string list * tyexpr
  | Trait of id * trait
  | Impl of id * impl
[@@deriving show { with_path = false }]

type file = {
  name : string;
  phys_path : string;
  toplevel : statement list;
  imports : path list;
  opens : path list;
}
[@@deriving show { with_path = false }]

let format_error (id : Common.Info.id) (err : string) =
  let span = Common.Info.get_property id Srcloc in
  match span with
  | None -> "Error: " ^ err ^ "\n <no line information> "
  | Some (Srcloc' s) ->
      "Error: "
      ^ err
      ^ " at: "
      ^ Format.sprintf "(%d:%d, %d:%d)\n" (fst s.col) (snd s.col)
          (fst s.row) (snd s.row)
      ^ BatText.to_string s.slice
  | _ -> failwith "bad property lookup"
