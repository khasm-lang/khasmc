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

let search (ctx : ctx) (id : resolved) : (resolved typ, string) result
    =
  match List.assoc_opt id ctx.locals with
  | Some ty -> ok ty
  | None -> (
      match List.assoc_opt id ctx.ctors with
      | Some t -> (
          match t.content with
          | Record r -> ok @@ TyCustom t.name
          | Sum s ->
              let l = List.assoc id s in
              ok @@ typ_list_to_typ l)
      | None -> (
          match List.assoc_opt id ctx.funs with
          | Some d -> ok @@ definition_type d
          | None -> (
              match List.assoc_opt id ctx.traitfuns with
              | Some t ->
                  let d =
                    List.find
                      (fun (d : ('a, 'b) definition) -> d.name = id)
                      t.functions
                  in
                  ok @@ definition_type d
              | None -> err ("no such thing: " ^ show_resolved id))))

let type_information : resolved typ by_uuid = new_by_uuid 100
let add_type uuid typ = Hashtbl.replace type_information uuid typ

let rec break_down_case_pattern (ctx : ctx) (c : resolved case)
    (t : resolved typ) :
    ((resolved * resolved typ) list, string) result =
  match c with
  | CaseVar v -> ok [ (v, t) ]
  | CaseTuple tu -> begin
      match t with
      | TyTuple t' ->
          List.map2 (break_down_case_pattern ctx) tu t'
          |> collect
          |> Result.map List.flatten
          |> Result.map_error (String.concat " ")
      | _ -> err "not tuple but should be tuple :("
    end
  | CaseCtor (name, args) -> failwith "ugh"

let rec infer (ctx : ctx) (e : resolved expr) :
    (resolved typ, string) result =
  let* ty =
    match e with
    | Var (i, v) -> search ctx v
    | Int (_, _) -> ok TyInt
    | String (_, _) -> ok TyString
    | Char (_, _) -> ok TyChar
    | Float (_, _) -> ok TyFloat
    | Bool (_, _) -> ok TyBool
    | LetIn (i, case, annot, head, body) -> failwith "let"
    | Seq (_, _, _) -> _
    | Funccall (_, _, _) -> _
    | Binop (_, _) -> _
    | Lambda (_, _, _, _) -> _
    | Tuple (_, _) -> _
    | Annot (_, _, _, _, _) -> _
    | Match (_, _, _) -> _
    | Project (_, _, _) -> _
    | Ref (_, _) -> _
    | Modify (_, _, _) -> _
    | Record (_, _) -> _
  in
  let uuid = get_uuid e in
  add_type uuid ty;
  ok ty

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
