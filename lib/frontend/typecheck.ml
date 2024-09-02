open Share.Uuid
open Ast
open Share.Result

type ctx = {
    (* name, parent *)
    ctors: (resolved * resolved typdef) list;
    types: resolved typdef list;
    traitfuns: (resolved * resolved trait) list;
    funs: (resolved * resolved definition) list;
    locals: (resolved * resolved typ) list;
    local_polys: resolved list;
  }

let empty_ctx () = {
    ctors = [];
    types = [];
    traitfuns = [];
    funs = [];
    locals = [];
    local_polys = [];
  }

let type_information : resolved typ by_uuid = new_by_uuid 100
let add_type uuid typ = Hashtbl.replace type_information uuid typ

let infer (ctx: ctx) (e: resolved expr):
      (resolved typ, string) result =
  failwith "ho"

let check (ctx: ctx) (e: resolved expr) (t: resolved typ):
      (resolved typ, string) result =
  failwith "hi"

let typecheck_definition (ctx: ctx) (d: resolved definition):
      (unit, string) result =
  let polys = d.typeargs in
  let args = d.args in
  let self = (d.name, d) in
  let ctx = { ctx with
              locals = args;
              local_polys = polys;
              funs = self :: ctx.funs; (* yay recursion *)
            }
  in
  let** body = (d.body, "should be body") in
  let* _ = check ctx body d.return in
  ok ()
  
let gather (t: resolved toplevel list): ctx =
  let ctx = empty_ctx () in
  List.fold_left (fun ctx a ->
      match a with
      | Typdef t -> begin
          match t.content with
          | Record r -> { ctx with
                         ctors = (t.name, t) :: ctx.ctors;
                         types = t :: ctx.types
                       }
          | Sum s -> List.fold_left (fun acc a ->
                        { acc with
                          ctors = (fst a, t) :: acc.ctors;
                        }
                      )
                      {ctx with types = t :: ctx.types} s 
        end
      | Trait t -> List.fold_left (fun acc (a: 'a definition) ->
                      { acc with
                        traitfuns = (a.name, t) :: acc.traitfuns
                      }
                    ) ctx t.functions
      | Impl _ -> (* we don't do anything here yet
                    TODO: typecheck impl'd functions
                  *)
         ctx
      | Definition d ->
         { ctx with
           funs = (d.name, d) :: ctx.funs
         }
    ) ctx t


let typecheck_toplevel (t: resolved toplevel list): unit =
  let ctx = gather t in
  failwith "temp"
