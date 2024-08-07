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

type path =
  (* used to represent a path that doesn't lead anywhere *)
  | End
  (* x *)
  | Base of string
  (* Foo. *)
  | InMod of string * path
[@@deriving show { with_path = false }]

let rec to_str (p : path) : string =
  match p with
  | Base n -> n
  | End -> "<END>"
  | InMod (s, pth) -> s ^ "." ^ to_str pth

let path_append (p : path) (s : string) : string = to_str p ^ "." ^ s

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
  | TyMeta of meta BatUref.t
      [@printer fun fmt t -> pp_meta fmt (BatUref.uget t)]
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
  | TForall of freevar list * ty

and meta =
  | Unsolved
  | Solved of ty

and freevar = string [@@deriving show { with_path = false }]

let rec force (t : ty) : ty =
  match t with
  | Tuple t -> Tuple (List.map force t)
  | Arrow (a, b) -> Arrow (force a, force b)
  | TApp (a, b) -> TApp (a, List.map force b)
  | TForall (s, p) -> TForall (s, force p)
  | TyMeta m -> (
      match BatUref.uget m with Unsolved -> t | Solved t -> force t)
  | _ -> t

let rec pp_meta (fmt : Format.formatter) (ty : meta) : unit =
  match ty with
  | Unsolved -> Format.fprintf fmt "Unsolved"
  | Solved t -> Format.fprintf fmt "Solved: (%a)" pp_ty t

and pp_ty (fmt : Format.formatter) (ty : ty) : unit =
  match ty with
  | TyString -> Format.fprintf fmt "String"
  | TyInt -> Format.fprintf fmt "Int"
  | TyBool -> Format.fprintf fmt "Bool"
  | TyChar -> Format.fprintf fmt "Char"
  | TyMeta m -> pp_meta fmt (BatUref.uget m)
  | Free s -> Format.fprintf fmt "'%s" s
  | Custom t -> Format.fprintf fmt "%a" pp_path t
  | Tuple t -> Format.fprintf fmt "(%a)" (pp_list fmt) t
  | Arrow (a, b) -> Format.fprintf fmt "(%a) -> %a" pp_ty a pp_ty b
  | TApp (p, l) ->
      Format.fprintf fmt "%a (%a)" pp_path p (pp_list fmt) l
  | TForall (s, t) ->
      Format.fprintf fmt "forall%a. %a"
        (fun fmt t -> List.iter (Format.fprintf fmt " %s") t)
        s pp_ty t

and pp_list fmt fmt x =
  List.iter
    (fun x ->
      pp_ty fmt x;
      Format.fprintf fmt ", ")
    x

let print_ty ty =
  pp_ty Format.std_formatter ty;
  Format.print_newline ();
  Format.print_flush ()

(* also carries free vars *)
type ty' = freevar list * ty [@@deriving show { with_path = false }]

let no_frees (ty : ty) : ty' = ([], ty)

type tyexpr =
  (* variant name, list of types *)
  | TVariant of (string * ty list) list
  (* field name, ty *)
  | TRecord of (string * ty) list
  (* just an alias for something *)
  | TAlias of ty
[@@deriving show { with_path = false }]

type constraint' = path * ty list
[@@deriving show { with_path = false }]

let pp_exn fmt exn = Format.fprintf fmt "%s" (Printexc.to_string exn)

type tm =
  | Bool of id * bool
  | Int of id * string
  | String of id * string
  | Char of id * string
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
  | Project of id * tm * string
[@@deriving show { with_path = false }]

type definition_no_body = {
  name : string;
  free_vars : freevar list;
  constraints : constraint' list;
  args : (string * ty) list;
  ret : ty;
  id : Common.Info.id;
}
[@@deriving show { with_path = false }]

type definition = {
  name : string;
  free_vars : freevar list;
  constraints : constraint' list;
  args : (string * ty) list;
  ret : ty;
  body : tm;
  id : Common.Info.id;
}
[@@deriving show { with_path = false }]

let to_definition_no_body (d : definition) : definition_no_body =
  match d with
  | { name; free_vars; constraints; id; args; ret; body = _body } ->
      { name; free_vars; constraints; id; args; ret }

let to_definition (b : tm) (d : definition_no_body) : definition =
  match d with
  | { name; free_vars; constraints; id; args; ret } ->
      { body = b; name; free_vars; id; constraints; args; ret }

type trait = {
  name : string;
  args : freevar list;
  (* any associated types *)
  assoc_types : freevar list;
  (* any constraints on said & on inputs *)
  constraints : constraint' list;
  (* member functions *)
  functions : definition_no_body list;
  id : Common.Info.id;
}
[@@deriving show { with_path = false }]

type impl = {
  name : string;
  args : (string * ty) list;
  assoc_types : (string * ty) list;
  impls : definition list;
  id : Common.Info.id;
}
[@@deriving show { with_path = false }]

type typ = {
  name : string;
  args : freevar list;
  expr : tyexpr;
  id : Common.Info.id;
}
[@@deriving show { with_path = false }]

type statement =
  (* name, freevars, constraints, args, return type, term *)
  | Definition of definition
  (* name, args, body *)
  | Type of typ
  | Trait of trait
  | Impl of impl
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

let[@tail_mod_cons] rec mk_ty (tyl : ty list) (r : ty) : ty =
  match tyl with [] -> r | x :: xs -> Arrow (x, mk_ty xs r)

let get_tm_id (t : tm) : id =
  match t with
  | Bool (i, _)
  | String (i, _)
  | Int (i, _)
  | Char (i, _)
  | Var (i, _)
  | Bound (i, _)
  | App (i, _, _)
  | Let (i, _, _, _)
  | Match (i, _, _)
  | Lam (i, _, _, _)
  | ITE (i, _, _, _)
  | Annot (i, _, _)
  | Record (i, _, _)
  | Project (i, _, _) ->
      i
