open Ast
(*
  TODO: this doesn't work with shadowing
*)

let table = Hashtbl.create ~random:true 100

let bind_node node typ =
  Hashtbl.add table (Hashtbl.hash node) typ

let get_type node =
  Hashtbl.find table (Hashtbl.hash node)

let show_table () =
  Hashtbl.iter (fun x y -> print_endline (string_of_int x);
                           print_endline (pshow_typesig y))
    table

let get_type_str node =
  pshow_typesig (get_type node)
