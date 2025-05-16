open Frontend.Ast
open Share.Maybe
open Share.Uuid

type m_name = resolved
type m_typ = resolved typ

type monomorph_info = {
  pre_monomorph : (m_name, (m_name, yes) definition) Hashtbl.t;
  monomorph_information :
    (m_name * m_typ, (m_name, yes) definition) Hashtbl.t;
}

let new_ctx () =
  {
    pre_monomorph = Hashtbl.create 100;
    monomorph_information = Hashtbl.create 100;
  }

let add_pre_monomorph top ctx =
  top
  |> List.iter (function
       | Definition def -> Hashtbl.add ctx.pre_monomorph def.name def
       | _ -> ())

let rec monomorph (ctx : monomorph_info) (def : (_, _) definition) typ
    : (_, yes) definition =
  let body = get def.body in
  let _body' =
    match body with _ -> failwith "todo: monomorph body"
  in
  failwith "todo"

and monomorph_get (ctx : monomorph_info) (def : (_, _) definition) typ
    : uuid =
  match
    Hashtbl.find_opt ctx.monomorph_information (def.name, typ)
  with
  | Some res -> res.data.uuid
  | None -> (
      let def = monomorph ctx def typ in
      try
        Hashtbl.add ctx.monomorph_information (def.name, typ) def;
        def.data.uuid
      with Stack_overflow ->
        failwith
          "you probably tried to recursively monomorphize something")

let monomorphize (top : resolved toplevel list) : monomorph_info =
  let ctx = new_ctx () in
  add_pre_monomorph top ctx;
  (* add everything to the queue *)
  (* TODO: Make this more advanced *)
  let main =
    match Hashtbl.find_opt ctx.pre_monomorph (R "main") with
    | Some def -> def
    | None -> failwith "main not found >:("
  in
  let _ = monomorph_get ctx main (TyArrow (TyTuple [], TyTuple [])) in
  ctx
