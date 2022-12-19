open Exp
open Typecheck_env
open Ast
open Uniq_typevars


type ctx = {
    vars : (fident * typesig) list;
    typs : string list;
  }
[@@deriving show {with_path=false}]

let emptyctx () = {vars = []; typs = []}

let assume ctx name typ = {ctx with vars = (name, typ) :: ctx.vars}

let assumeT ctx name = {ctx with typs = name :: ctx.typs}

let rec lookupT ctx name =
  try
    Some(snd (List.find (fun x -> (fst x) = name) ctx.vars))
  with
  | Not_found ->
     None
         
let isTvar ctx name =
  List.mem name ctx.typs


let rec subs typ name new' =
  match typ with
  | TSBase(x) -> if x = name then new' else typ
  | TSApp(x, y) -> TSApp(subs x name new', y)
  | TSMap(x, y) -> TSMap(subs x name new', subs y name new')
  | TSForall(x, y) ->
     if x = name then
       typ
     else
       TSForall(x, subs y name new')
  | TSTuple(l) -> TSTuple(List.map (fun x -> subs x name new') l)


let rec type_eq t1 t2 =
  match (t1, t2) with
  | (TSBase(x), TSBase(y)) -> x = y
  | (TSMap(x, y), TSMap(a, b)) -> type_eq x a && type_eq y b
  | (TSApp(x, y), TSApp(a, b)) -> type_eq x a && y = b
  | (TSForall(x, y), TSForall(a, b)) ->
     type_eq (subs b a (TSBase(x))) y
  | (TSTuple(x), TSTuple(y)) ->
     List.for_all2 (fun x y -> type_eq x y) x y
  | (_, _) -> false

let conv x y =
  if type_eq x y then
    ()
  else
    raise (TypeErr
             (
               "These types:\n'"
               ^ pshow_typesig x
               ^ "' and '"
               ^ pshow_typesig y
               ^ "'\nmust be equal, but are not"
      ))

let rec lookupBase ctx x =
  match x with
  | Ident(f) ->
     begin
       match lookupT ctx f with
       | Some(x) -> x
       | None -> raise (TypeErr("undefined ident: " ^ str_of_fident f))
     end
  | Int(_) -> TSBase("int")
  | Float(_) -> TSBase("float")
  | Str(_) -> TSBase("string")
  | True | False -> TSBase("bool")
  | Tuple(l) -> TSTuple(List.map (fun x -> infer ctx x) l)

and infer ctx term =
  print_endline "\n\n\nINFER:\n";
  print_endline (show_ctx ctx);
  print_endline (show_kexpr term);
  match term with
  | Base(x) -> lookupBase ctx x
  | Paren(x) -> infer ctx x
  | FCall(f, a) ->
     begin
       let typ1 = infer ctx f in
       match typ1 with
       | TSMap(l1, r1) -> check ctx a l1; r1
       | TSForall(_, _) ->
          let typ2 = infer ctx (Inst(f, infer ctx a)) in
          typ2
       | _ -> raise (TypeErr "cannot apply non-function type")
     end
  | Inst(f, t) ->
     let termtp = infer ctx f in
     begin
       match termtp with
       | TSForall(v, m) -> subs m v t
       | _ -> raise (TypeErr "Can't instantiate a non-forall")
     end
  | LetIn(nm, e1, e2) ->
     let body = infer ctx e1 in
     let ass = assume ctx (Bot(nm)) body in
     infer ass e2
  | AnnotLam(x, t, b) ->
     let out = infer (assume ctx (Bot(x)) t) b in
     TSMap(t, out)
  | AnnotLet(i, t, e1, e2) ->
     check ctx e1 t;
     let ass = assume ctx (Bot(i)) t in
     infer ass e2
  | IfElse(_, e1, e2) ->
     begin
       try
         let typ = infer ctx e1 in
         check ctx e2 typ;
         typ
       with
       | TypeErr(_) ->
          let typ = infer ctx e2 in
          check ctx e1 typ;
          typ
     end
  | TupAccess(tm, i) ->
     begin
       let typ = infer ctx tm in
       match typ with
       | TSTuple(x) ->
          List.nth x i
       | _ -> raise (TypeErr("Can't project from non-tuple:\n"
                             ^ show_kexpr tm
                             ^ "\nwith type:\n"
                             ^ pshow_typesig typ))
     end
  | TypeLam(t, e) ->
     let typ = infer (assumeT ctx t) e in
     TSForall(t, typ)
  | _ -> raise (TypeErr ("Cannot infer:\n\n"
                         ^ show_kexpr term
                         ^ "\n\nMaybe add annotations?"))

and check ctx term typ =
  print_endline "\n\n\nCHECK:\n";
  print_endline (show_ctx ctx);
  print_endline (show_kexpr term);
  print_endline (pshow_typesig typ);
  match (term, typ) with
  | (Lam(e, e'), TSMap(x, y)) ->
     check (assume ctx (Bot(e)) x) e' y
  | (TypeLam(t, e), TSForall(nm, typ)) ->
     let typ' = subs typ nm (TSBase(t)) in
     check (assumeT ctx t) e typ'
  | (LetIn(i, e, e'), b) ->
     let body = infer ctx e in
     let ass = assume ctx (Bot(i)) body in
     check ass e' b
  | (Base(x), y) ->
     conv (infer ctx (Base(x))) y
  | (IfElse(cond, e1, e2), t) ->
     check ctx cond (TSBase("bool"));
     check ctx e1 t;
     check ctx e2 t
  | (tm, exp) ->
     let act = infer ctx tm in
     conv exp act
