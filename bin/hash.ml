open Ast
(*
  TODO: this doesn't work with shadowing
*)

let table =
  let t = Hashtbl.create ~random:true 100 in
  Hashtbl.add t (-1) (TSBottom);
  t

let bind_node node typ =
  Hashtbl.add table node typ
(*bind node info*)
let bni node typ =
  Hashtbl.add table node.id typ

let get_type node =
  Hashtbl.find table node

let show_table () =
  Hashtbl.iter (fun x y -> print_string (string_of_int x);
                           print_string " : ";
                           print_endline (pshow_typesig y))
    table

let get_type_str node =
  pshow_typesig (get_type node)

(*get str type*)
let gst nd = get_type_str nd.id
