open Ast

let print_int x = print_endline (string_of_int x)

let print_bool x = print_endline (string_of_bool x)

let rec uniq x =
  let rec uniq_help l n =
    match l with
    | [] -> []
    | h :: t -> if n = h then uniq_help t n else h::(uniq_help t n) in
  match x with
  | [] -> []
  | h::t -> h::(uniq_help (uniq t) h)

type unity = {
    parent : unity option;
    join : (string * typesig) list;
    foralls : string list;
  } [@@deriving show {with_path = false}]

let new_unity p = 
  {parent = p; join = []; foralls = [];}

let add_forall strl un =
  {un with foralls = uniq (strl @ un.foralls)}

let rec is_forall str un =
  match List.filter (fun x -> x = str) un.foralls with
  | _x :: _xs -> true
  | [] ->
     match un.parent with
     | None -> false
     | Some(x) -> is_forall str x

let rec find_alias_in_unity str un =
  match List.filter (fun x -> (fst x) = str) un.join with
  | x :: xs -> Some(x :: xs)
  | [] ->
     match un.parent with
     | None -> None
     | Some(x) -> find_alias_in_unity str x

exception SubtypeOfFailure of string

let add_to_unity s t un =
  let already = find_alias_in_unity s un in
  begin
    match already with
    | Some(al) ->
       List.iter
         (fun x ->
           if (snd x) = t then () else raise (SubtypeOfFailure ("binding already present for tsvar " ^ s)))
         al;
    | None -> ()
  end;
  {un with join = (s, t) :: un.join;}

let combine unb una =
  {una with join = uniq (una.join @ unb.join); foralls = uniq (una.foralls @ unb.foralls)}



let rec subtype_base (x: ktype) (y: ktype) ctx =
  match (x, y) with
  | (KTypeBasic(a), KTypeBasic(b)) ->
     begin
       if is_forall a ctx then
         combine (add_to_unity a (TSBase(KTypeBasic(b))) ctx) ctx
       else
         if a = b then ctx else raise (SubtypeOfFailure ("incompat types: " ^ a ^ " " ^ b))
     end
  | (KTypeApp(a1, a2), KTypeApp(b1, b2)) ->
     begin
       let more1 = subtype_of a1 b1 ctx in
       if a2 = b2 then
         combine more1 ctx
       else
         raise (SubtypeOfFailure ("incompat types: " ^ a2 ^ " " ^ b2))
     end
  | (KTypeBasic(a), KTypeApp(b1, b2)) ->
     begin
       if is_forall a ctx then
         combine (add_to_unity a (TSBase(KTypeApp(b1, b2))) ctx) ctx
       else
         raise (SubtypeOfFailure ("incompatible types"))
     end
  | (KTypeApp(_a1, _a2), KTypeBasic(_b)) ->
     raise (SubtypeOfFailure("Cannot unify application of typecon with base type"))


and subtype_of x y ctx =
  match (x, y) with
  | (TSBase(a), TSBase(b)) -> combine (subtype_base a b ctx) ctx
  | (TSMap(a1, a2), TSMap(b1, b2)) -> 
     begin
       let temp = combine (subtype_of a1 b1 ctx) ctx in
       combine (subtype_of a2 b2 temp) temp
     end
  | (TSForall(fa, ta), TSForall(_fb, tb)) ->
     begin
       let newctx = add_forall fa ctx in
       combine (subtype_of ta tb newctx) ctx
     end
  | (TSTuple(a), TSTuple(b)) ->
     combine (subtype_tuples a b ctx) ctx
  | (TSForall(f, t), _) ->
     begin
       let newctx = add_forall f ctx in
       combine (subtype_of t y newctx) ctx
     end
  | (TSBase(KTypeBasic(a)), _) ->
     if is_forall a ctx then
       combine (add_to_unity a y ctx) ctx
     else
       raise (SubtypeOfFailure "cannot subtype_of base and non-base w/o polymorhpism")
  | (_, _) -> raise (SubtypeOfFailure "invalid unification")

and subtype_tuples x y ctx =
  match (x, y) with
  | (b :: bs, c :: cs) ->
     let newctx = subtype_of b c ctx in
     combine (subtype_tuples bs cs newctx) ctx
  | ([], []) -> ctx
  | (_, _) -> raise (SubtypeOfFailure "unequal tuple lengths")


let (<@@>) a b = subtype_of a b
