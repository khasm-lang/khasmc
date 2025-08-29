open Frontend.Ast
open Frontend.Typecheck
open Trait_resolution.Resolve
open Monomorphize
open Share.Uuid

let hashtbl_map f tbl =
  let tbl' = Hashtbl.create (Hashtbl.length tbl) in
  Hashtbl.iter
    (fun a b ->
      let c, d = f a b in
      Hashtbl.add tbl' c d)
    tbl;
  tbl'

let rec propagate_h (name, typ) (uuid, def_l) :
    (m_name * resolved typ) * (uuid * (_, _) definition) =
  (* all of the definitions should already be forced, so *)
  if not (Lazy.is_val def_l) then
    failwith "propagate_h stuff should def. be a value by now";
  let def = Lazy.force def_l in

  ((name, typ), (uuid, def))

let propagate (ctx : monomorph_info) =
  hashtbl_map propagate_h ctx.monomorph_information
