open Share.Uuid
open Share.Maybe
open Share.Types       
module P = Parsing.Ast
module IR = IR             
type name = IR.name

type poly_ctx = {
  poly_constructors :
    (name, name P.typdef) Hashtbl.t;
  poly_records :
    (name, name P.typdef) Hashtbl.t;
}

type ctx = {
  constructor_map :
    (name * IR.typ, name) Hashtbl.t;
  record_map :
    (name * IR.typ, name) Hashtbl.t;
}

let find_all_mono_types (polys : poly_ctx)
    (top : (_, _) P.expr) = failwith "tmp"

let convert_to_flat (tops : (P.resolved, 'b, void) P.toplevel list) :
  yes IR.program =
  let prog = {
    IR.toplevel = [];
    IR.records = [];
    IR.constructors = [];
  } in
  let with_polys = { poly_constructors = Hashtbl.create 100;
                     poly_records = Hashtbl.create 100 }
  in 
  let () = List.iter (fun (p : ('a,'b,void) P.toplevel) ->
      match p with
      | P.Typdef t -> begin match t.content with
          | Record r ->
            Hashtbl.add with_polys.poly_records t.name t
          | Sum s ->
            Hashtbl.add with_polys.poly_constructors t.name t
        end 
      | P.Definition _ -> ()
    ) tops in
  List.fold_left (fun acc (p : ('a,'b,void) P.toplevel) ->
      match p with
      | P.Typdef typ -> failwith "handle type"
      | P.Definition d -> failwith "handle definition"
    ) prog tops

