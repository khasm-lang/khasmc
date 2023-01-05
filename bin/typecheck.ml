open Exp
open Ast
open Uniq_typevars
open Debug

type ctx = {
    binds : (kident * typesig) list;
  }
[@@deriving show {with_path = false}]

let empty_typ_ctx () = {binds=[
                          (*hardcode in that () is bottom*)
                          ("()", TSBottom)
                       ]}

let assume_typ ctx id ts =
  {binds = (id, make_uniq_ts ts None) :: ctx.binds}


let rec occurs_ts s ts =
  (* checks whether a variable s is free in ts *)
  match ts with
  | TSBase(x) -> x = s
  | TSBottom -> false
  | TSMeta(_) -> false
  | TSApp(x, _) -> occurs_ts s x
  | TSMap(x, y) -> occurs_ts s x || occurs_ts s y
  | TSForall(x, y) -> if x = s then false else occurs_ts s y
  | TSTuple(x) ->
     List.map (fun x -> occurs_ts s x) x
     |> List.mem true

let rec lift_ts_h t =
  match t with
  | TSBase(_) -> (t, false)
  | TSBottom -> (t, false)
  | TSMeta(_) -> (t, false)
  | TSApp(x, y) ->
     let did = lift_ts_h x in
     (TSApp(fst did, y), snd did)
  | TSMap(x, TSForall(f, y)) ->
     if occurs_ts f x then
       let left = lift_ts_h x in
       let right = lift_ts_h (TSForall(f, y)) in
       (TSMap(fst left, fst right), snd left || snd right)
     else
       (TSForall(f, TSMap(x, y)), true)
  | TSMap(x, y) ->
     let left = lift_ts_h x in
     let right = lift_ts_h y in
     (TSMap(fst left, fst right), snd left || snd right)
  | TSForall(x, TSMap(l, TSForall(y, r))) ->
     let l' = lift_ts_h l in
     let r' = lift_ts_h r in
     if occurs_ts y l then
       (
         TSForall(x, TSMap(fst l', TSForall(y, fst r')))
       ,
         snd l' || snd r'
       )
     else
       (
         TSForall(x, TSForall(y, TSMap(fst l', fst r')))
       ,
         true
       )
  | TSForall(x, y) ->
     let r' = lift_ts_h y in
     (TSForall(x, fst r'), snd r')
  | TSTuple(t) ->
     let hm = List.map lift_ts_h t in
     (TSTuple(List.map fst hm), List.mem true (List.map snd hm))

let rec lift_ts ts =
  (* lifts types like
     ∀a, a -> (∀b, b)
     to
     ∀a b, a -> b

     and simpls, like
     ∀a b, a -> a
     to
     ∀a, a -> a
   *)
  match lift_ts_h ts with
  | (t, false) -> t
  | (t, true) -> lift_ts t


let lookup ctx x =
  match List.find_opt (fun y -> fst y = x) ctx.binds with
  | None -> raise (NotFound(x ^ " not found in ctx"))
  | Some(x) -> snd x

let rec subs typ nm newt =
  match typ with
  | TSBottom -> TSBottom
  | TSBase(x) ->
     if x == nm then
       newt
     else
       typ
  | TSMeta(_) -> typ
  | TSApp(x, y) -> TSApp(subs x nm newt, y)
  | TSMap(l, r) -> TSMap(subs l nm newt, subs r nm newt)
  | TSForall(nm', ts) ->
     if nm' == nm then
       typ
     else
       TSForall(nm', subs ts nm newt)
  | TSTuple(l) -> TSTuple(List.map (fun x -> subs x nm newt) l)

type unify_ctx = {
    metas : (string * typesig) list;
  }
[@@deriving show {with_path = false}]

let empty_unify_ctx () = {metas = []}

let get_meta_opt ctx m =
  List.find_opt (fun x -> fst x = m) ctx.metas 

let lookup_meta ctx m =
  match List.find_opt (fun x -> fst x = m) ctx.metas with
  | Some(x) -> Some(snd x)
  | None -> None


let rec inst_meta tp orig meta =
  match tp with
  | TSBottom -> TSBottom
  | TSBase(x) -> if x = orig then meta else tp
  | TSMeta(_) -> tp
  | TSApp(ts, p) -> TSApp(inst_meta ts orig meta, p)
  | TSMap(a, b) -> TSMap(inst_meta a orig meta, inst_meta b orig meta)
  | TSForall(f, x) ->
     if f = orig then
       tp
     else
       TSForall(f, inst_meta x orig meta)
  | TSTuple(t) -> TSTuple(List.map (fun x -> inst_meta x orig meta) t)

let rec inst_all ts =
  match ts with
  | TSForall(fv, bd) ->
     let meta = TSMeta(get_meta ()) in
     let new' = inst_meta bd fv meta in
     inst_all new'
  | TSApp(x, y) -> TSApp(x, y)
  | TSMap(x, y) -> TSMap(x, y)
  | TSBottom -> TSBottom
  | TSBase(x) -> TSBase(x)
  | TSMeta(x) -> TSMeta(x)
  | TSTuple(t) -> TSTuple(List.map inst_all t)

let uniq_cons x xs = if List.mem x xs then xs else x :: xs

let mk_uniq_list xs = List.fold_right uniq_cons xs []

let combine_uniq x y =
  mk_uniq_list (x @ y)

let combine ctx1 ctx2 =
  let rec helper a b =
    match a with
    | [] -> b
    | x :: xs ->
       match List.find_opt (fun y -> fst y = fst x) b with
       | None -> x :: helper xs b
       | Some(k) ->
          if snd k = snd x then
            helper xs b
          else
            raise (UnifyErr ("metavariable " ^ fst x ^ " has both"
                             ^ " type "
                             ^ pshow_typesig (snd k)
                             ^ " and "
                             ^ pshow_typesig (snd x)
                             ^ " within unification"))
  in
  {metas = helper ctx1.metas ctx2.metas}

let rec unify_list ctx l1 l2 =
  match (l1, l2) with
  | ([], []) -> (ctx, [])
  | (x :: xs, y :: ys) ->
     let tmp = unify ctx x y in
     let rest = unify_list (fst tmp) xs ys in
     ((fst rest), (snd tmp) :: (snd rest))
  | (_, _) -> raise (UnifyErr ("unequal tuple len"))


(* TODO
Credit to AradArbel10 on rpl

if you're doing this in ocaml (or any language with ref cells) there's
actually a really neat trick to optimize (and simplify 😄) metavars.
```
type meta_state =
| Solved of typ
| Unsolved of name (* used for pretty printing *)
and typ =
| Arrow of typ * typ
| ...
| Meta of meta_state ref
```
so now the metavar is mutable, and when you solve it it gets
"propagated up" automatically. actually implementing it introduces a
couple of other subtleties but I think it's worth it.
 *)

(*
  MetaVar strategy:
```
given f x
instantiate f with metavariables until reaching a non-forall type
check that f's type matches 'a -> 'b
unify x's type and 'a
apply that unification to 'b
return that type
```
so, for example:
```
f : ∀'a, 'a -> 'a
x : float

f x
→ 
META_INST(f) : $m -> $m
→
unify LHS(f) and x, ∴ $m = float
→
apply to RHS(f)
∴ 
f : float -> float 
(in this application)
→
return RHS(f) : float
```


 *)

(*
  returns a tuple of env, typ 
 *)

and unify ?(loop) ctx l r =
  (*
    unification takes something with metavariables, eg
    $m1 -> $m1

    and figures out what the metavariables should be:

    unify
    $m1 -> $m1
    int -> int
    ∴
    $m1 = int

    it does this by returning a tuple of (ctx, tm)

    where the ctx contains metavariable info.
    on a map, it can then check that the metavar info is the same on
    both sides, and go forth as such.


    note that metavars can appear on both sides -
    this is why there's a default case at the bottom to switch
    the arguments around, using an optional param to make sure
    that it doesn't loop forever (way easier then rewriting the
    logic for both sides lol)

   *)
  debug "\n\n(\nUNIFY";
  let l = lift_ts l in
  let r = lift_ts r in
  debug (pshow_typesig l);
  debug (pshow_typesig r);
  let res = match (l, r) with
    | (TSBase(x), TSBase(y)) ->
       debug "BASE";
       if x = y then
         (ctx, TSBase(x))
       else
         raise (UnifyErr ("can't unify " ^ x ^ " and " ^ y))
    | (TSMap(a, b), TSMap(x, y)) ->
       debug "MAP";
       let lt = unify ctx a x in
       let rt = unify ctx b y in
       (combine (fst lt) (fst rt), TSMap((snd lt), (snd rt)))
    | (TSApp(a, b), TSApp(x, y)) ->
       debug "APP";
       if b <> y then
         raise (UnifyErr ("can't unify" ^ b ^ " and " ^ y))
       else
         let t = unify ctx a x in
         (fst t, TSApp(snd t, b))
    | (TSForall(a, b), TSForall(x, y)) ->
       debug "FORALL";
       (*
         TODO: show how this works
        *)
       let sub = subs y x (TSBase(a)) in
       let met = subs b a (TSMeta(get_meta ())) in
       let unf = unify ctx met sub in 
       (fst unf, TSForall(a, snd unf))
    | (TSTuple(a), TSTuple(x)) ->
       debug "TUPLE";
       let tmp = unify_list ctx a x in
       (fst tmp, TSTuple(snd tmp))
    | (TSBottom, TSBottom) ->
       debug "BOT";
       (ctx, TSBottom)
    | (TSMeta(m), _) ->
       debug "META";
       begin
         match get_meta_opt ctx m with
         | Some(x) ->
            let typ = snd x in
            unify ctx typ r
         | None ->
            (
              combine {metas = [(m, r)]} ctx,
              r
            )
       end
    | (_, _) ->
       match loop with
       | None -> unify ~loop:true ctx r l
       | Some(_) -> 
          raise (UnifyErr
                   ("Can't unify "
                    ^ pshow_typesig l
                    ^ " and "
                    ^ pshow_typesig r
            ))
  in
  let res = (fst res, lift_ts (snd res)) in
  debug "\n\nUNIFY RESULT:";
  debug (show_unify_ctx (fst res));
  debug (pshow_typesig (snd res));
  debug "\n)\n";
  res

let rec apply_unify ctx tp =
  match tp with
  | TSBottom -> TSBottom
  | TSBase(_) -> tp
  | TSMeta(t) -> 
     begin
       match lookup_meta ctx t with
       | Some(x) -> x
       | None -> tp
     end
  | TSApp(f, x) -> TSApp(apply_unify ctx f, x)
  | TSMap(a, b) -> TSMap(apply_unify ctx a, apply_unify ctx b)
  | TSForall(f, x) -> TSForall(f, apply_unify ctx x)
  | TSTuple(t) -> TSTuple(List.map (apply_unify ctx) t)




let rec infer_base ctx tm =
  match tm with
  | Ident(i) -> lookup ctx i
  | Int(_) -> TSBase("int")
  | Float(_) -> TSBase("float")
  | Str(_) -> TSBase("string")
  | Tuple(l) -> TSTuple(List.map (fun x -> (infer ctx x)) l)
  | True | False -> TSBase("bool")

and infer ctx tm =
  (*
    infer the type of a term
   *)
  debug "\n\n(\nINFER";
  debug (show_kexpr tm);
  let res = match tm with
  | Base(x) -> infer_base ctx x
  | FCall(f, x) ->
     debug "\n\nFCALL";
     debug (show_kexpr f);
     debug (show_kexpr x);
     (*

       alright, so this mess. Basically what this does is
       infer instation of a function - ie, it takes
       f x
       and turns it into
       f [typeof x] x

       but it's not always that simple
       does this by inserting metavariables into all the foralls,
       then unifying the LHS of the function with the argument,
       then applying that to the RHS.

      *)
     begin
       let typ_r = (infer ctx f) in
       let inst_r = inst_all typ_r in
       let typ_l = (infer ctx x) in
       let inst_l = inst_all typ_l in
       debug ("INST r. : " ^ pshow_typesig inst_r);
       debug ("INST l. : " ^ pshow_typesig inst_l);
       match inst_r with
       | TSMap(a, b) ->
          let res = unify (empty_unify_ctx()) a inst_l in
          apply_unify (fst res) b
       | tp -> raise (TypeErr ("Cannot apply \n"
                              ^ show_kexpr x
                              ^ " to \n"
                              ^ show_kexpr f
                              ^ "\n of type: "
                              ^ pshow_typesig tp))
     end
  (*
    try and infer/check one side against the other and vice versa
   *)
  | IfElse(c, e1, e2) ->
     ignore (check ctx c (TSBase("bool")));
     begin
       try
         let typ = infer ctx e1 in
         ignore (check ctx e2 (typ));
         ( typ)
       with
       | TypeErr(_) ->
          let typ = infer ctx e2 in
          ignore (check ctx e1 ( typ));
          (typ)
     end
  | LetIn(id, e1, e2) ->
     begin
       let bodytyp =  (infer ctx e1) in
       let intyp = (infer (assume_typ ctx id bodytyp) e2) in
       intyp
     end
  | Join(a, b) ->
     ignore (check ctx a TSBottom);
     ((infer ctx b))
  | Inst(_, _) ->
     raise (TypeErr "UNREACHABLE")
  | TypeLam(t, b) ->
     let bodytyp = (infer ctx b) in
     (TSForall(t, bodytyp))
  | TupAccess(expr, i) ->
     begin
       match (infer ctx expr) with
       | TSTuple(t) -> List.nth t i
       | _ -> raise (TypeErr (
                         "can't tuple access non-tuple:\n"
                         ^ show_kexpr expr
                ))
     end
  | AnnotLet(id, ts, e1, e2) ->
     ignore (check ctx e1 ts);
     let ctx' = assume_typ ctx id ts in
     ((infer ctx' e2))
  | AnnotLam(id, ts, e) ->
     let out = (infer (assume_typ ctx id ts) e) in
     (TSMap(ts, out))
  | _ -> raise (TypeErr (
                    "Cannot infer:\n"
                    ^ show_kexpr tm
                    ^ "\nMaybe add annotations?"
           ))
  in
  let res = lift_ts res in
  debug "\n\nINFER RES:";
  debug (show_kexpr tm);
  debug ":";
  debug (pshow_typesig res);
  debug "\n)\n";
  res 

and check ctx tm tp =
  (*
    check a type against a term
   *)
  debug "\n\n(\nCHECK";
  debug (show_kexpr tm);
  debug (pshow_typesig tp); 
  begin
    match (tm, tp) with
    | (Lam(id, bd), TSMap(a, b)) ->
      ignore (check (assume_typ ctx id a) bd b);

     | (TypeLam(a, b), TSForall(fv, bd)) ->
         let typ' = subs bd fv (TSBase(a)) in
         ignore (check ctx b typ');

     | (LetIn(id, e1, e2), bd) ->
        let bdtyp = (infer ctx e1) in
        ignore (check (assume_typ ctx id bdtyp) e2 bd);

      | (Base(Tuple(x)), TSTuple(ts)) ->
        List.iter2 (fun x y -> ignore (check ctx x y)) x ts;

     | (term, exp) ->
        let actual = (infer ctx term) in
        ignore (unify (empty_unify_ctx ()) ( actual) exp);
  end;
  debug "\n)\n CHECK END"



let rec typecheck_toplevel_list ctx tl =
  match tl with
  | [] -> ctx
  | x :: xs ->
     let ctx' =
       match x with
       | TopAssign((id, ts), (_, args, body)) ->
          (*
            The purpose of this is to transform arguments into
            typelams and annotlams so that you can do
            sig ∀a b, a -> (a -> b) -> b in
            let apply x f = f x

            =>

            sig ∀a b, a -> (a -> b) -> b in
            let apply =
            ΛΑ =>
            ΛΒ =>
            λx : A =>
            λf : A -> B =>
            f x
            
           *)
          let rec forall_to_typelam ts args body =
            match (ts, args) with
            | (TSForall(fv, bd), _ :: xs) ->
               let tmp = forall_to_typelam bd xs body in
               (fst tmp, TypeLam(fv, snd tmp))
            | _ -> (lift_ts ts, body)
          in
          let rec add_args ts args body =
            match (ts, args) with
            | (TSMap(a, b), x :: xs) -> AnnotLam(x, a, add_args b xs body)
            | (_, [x]) -> AnnotLam(x, ts, body)
            | (_, []) -> body
            | (_, _) -> raise (TypeErr ("Cannot match args: " ^ (String.concat ", " args )
                     ^ " with typesig " ^ pshow_typesig ts))
          in
          let fixed = forall_to_typelam ts args body in
          let args_fixed = add_args (fst fixed) args body in
          let fixed_2 = forall_to_typelam ts args args_fixed in
          check ctx (snd fixed_2) ts;
          assume_typ ctx id ts
       (*
         assume the type is correct
        *)
       | Extern(id, ts) ->
          assume_typ ctx id ts
     in
     typecheck_toplevel_list ctx' xs

let typecheck_program p ctx =
  match p with
  | Program(tl) -> typecheck_toplevel_list ctx tl

let rec typecheck_program_list_h pl ctx =
  let ctx' = match ctx with
    | Some(x) -> x
    | None -> empty_typ_ctx ()
  in
  match pl with
  | [] -> ()
  | x :: xs ->
     let ctx'' = typecheck_program x ctx' in
     typecheck_program_list_h xs (Some (ctx''))

let typecheck_program_list pl = typecheck_program_list_h pl None
