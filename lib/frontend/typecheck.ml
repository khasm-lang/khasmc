open Share.Uuid
open Ast
open Share.Result
open Share.Maybe

type ctx = {
  (* name, parent *)
  ctors : (resolved * resolved typdef) list;
  types : resolved typdef list;
  traitfuns : (resolved * resolved trait) list;
  funs : (resolved * (resolved, no) definition) list;
  locals : (resolved * resolved typ) list;
  local_polys : resolved list;
}

let empty_ctx () =
  {
    ctors = [];
    types = [];
    traitfuns = [];
    funs = [];
    locals = [];
    local_polys = [];
  }

let add_local ctx a t = { ctx with locals = (a, t) :: ctx.locals }
let add_locals ctx t = { ctx with locals = t @ ctx.locals }

exception Case
exception DoneTy of resolved typ

let case' (type t) v f =
  match v with Some s -> raise @@ DoneTy (f s) | None -> ()

let search (ctx : ctx) (id : resolved) : (resolved typ, string) result
    =
  try
    case' (List.assoc_opt id ctx.locals) (fun t -> t);

    case' (List.assoc_opt id ctx.ctors) (fun t ->
        match t.content with
        | Record _ -> failwith "shouldn't be variable-ing a record"
        | Sum s -> typ_list_to_typ @@ List.assoc id s);

    case' (List.assoc_opt id ctx.funs) (fun d -> definition_type d);

    case' (List.assoc_opt id ctx.traitfuns) (fun t ->
        let d =
          List.find
            (fun (d : ('a, 'b) definition) -> d.name = id)
            t.functions
        in
        definition_type d);

    failwith "variable not found"
  with DoneTy s -> ok s

let type_information : resolved typ by_uuid = new_by_uuid 100
let add_type uuid typ = Hashtbl.replace type_information uuid typ

let rec break_down_case_pattern (ctx : ctx) (c : resolved case)
    (t : resolved typ) :
    ((resolved * resolved typ) list, string) result =
  let break_and_map a b =
    List.map2 (break_down_case_pattern ctx) a b
    |> collect
    |> Result.map List.flatten
    |> Result.map_error (String.concat " ")
  in
  match c with
  | CaseVar v -> ok [ (v, t) ]
  | CaseTuple tu -> begin
      match t with
      | TyTuple t' ->
          (* find all subpatterns *)
          break_and_map tu t'
      | _ -> err "not tuple but should be tuple :("
    end
  | CaseCtor (name, args) -> begin
      match t with
      (* TODO: this has a bunch of "assertions" in it, mostly around
         the fact that it assumes that type arguments are properly
         filled in with the righ tnumber of them and whatnot
         - that can obviously be false, so fix that
         - ie don't use `combine` and `find` mostly
      *)
      | TyCustom (head, targs) ->
          (* find ty*)
          let ty =
            List.find (fun (x : 'a typdef) -> x.name = head) ctx.types
          in
          begin
            match ty.content with
            | Record _ -> failwith "shouldn't be a record (fun)"
            | Sum s ->
                (* find constructor *)
                let ctor = List.find (fun x -> fst x = name) s in
                let map = List.combine ty.args targs in
                (* fill in all the type arguments *)
                let inst = List.map (instantiate map) (snd ctor) in
                break_and_map args inst
          end
      | _ -> err "not custom but should be"
    end

let test () = print_endline "nah!"

let rec infer (ctx : ctx) (e : resolved expr) :
    (resolved typ, string) result =
  let* ty =
    match e with
    (* try find that thing *)
    | Var (i, v) -> search ctx v
    | Int (_, _) -> ok TyInt
    | String (_, _) -> ok TyString
    | Char (_, _) -> ok TyChar
    | Float (_, _) -> ok TyFloat
    | Bool (_, _) -> ok TyBool
    | LetIn (i, case, annot, head, body) ->
        (* if there's an annot, check, else infer *)
        let* head'ty =
          match annot with
          | Some ty -> check ctx head ty
          | None -> infer ctx head
        in
        (* get everything out *)
        let* vars = break_down_case_pattern ctx case head'ty in
        let ctx' = add_locals ctx vars in
        infer ctx' body
    | Seq (_, a, b) ->
        (* we know the first branch must be unit *)
        let* _ = check ctx a (TyTuple []) in
        infer ctx b
    | Funccall (_, a, b) ->
        let* a'ty = infer ctx a in
        (* *)
        begin
          match a'ty with
          | TyArrow (q, w) -> failwith "tmp"
          | _ -> err "must function call on function type"
        end
    | Binop (_, _) -> failwith "tmp"
    | Lambda (_, _, _, _) -> failwith "tmp"
    | Tuple (_, _) -> failwith "tmp"
    | Annot (_, _, _) -> failwith "tmp"
    | Match (_, _, _) -> failwith "tmp"
    | Project (_, _, _) -> failwith "tmp"
    | Ref (_, _) -> failwith "tmp"
    | Modify (_, _, _) -> failwith "tmp"
    | Record (_, _, _) -> failwith "tmp"
  in
  let uuid = get_uuid e in
  add_type uuid ty;
  ok (force ty)

and check (ctx : ctx) (e : resolved expr) (t : resolved typ) :
    (resolved typ, string) result =
  failwith "hi"

let typecheck_definition (ctx : ctx) (d : (resolved, yes) definition)
    : (unit, string) result =
  let polys = d.typeargs in
  let args = d.args in
  let self = (d.name, forget_body d) in
  let ctx =
    {
      ctx with
      locals = args;
      local_polys = polys;
      (* yay recursion *)
      funs = self :: ctx.funs;
    }
  in
  let body = get d.body in
  let* _ = check ctx body d.return in
  ok ()

let gather (t : resolved toplevel list) : ctx =
  let ctx = empty_ctx () in
  List.fold_left
    (fun ctx a ->
      match a with
      | Typdef t -> begin
          match t.content with
          | Record r ->
              {
                ctx with
                ctors = (t.name, t) :: ctx.ctors;
                types = t :: ctx.types;
              }
          | Sum s ->
              List.fold_left
                (fun acc a ->
                  { acc with ctors = (fst a, t) :: acc.ctors })
                { ctx with types = t :: ctx.types }
                s
        end
      | Trait t ->
          List.fold_left
            (fun acc (a : ('a, 'b) definition) ->
              { acc with traitfuns = (a.name, t) :: acc.traitfuns })
            ctx t.functions
      | Impl _ ->
          (* we don't do anything here yet
             TODO: typecheck impl'd functions
          *)
          ctx
      | Definition d ->
          { ctx with funs = (d.name, forget_body d) :: ctx.funs })
    ctx t

let typecheck_toplevel (t : resolved toplevel list) : unit =
  let ctx = gather t in
  failwith "temp"
