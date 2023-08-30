let main_tbl : (int, Ast.typesig) Hashtbl.t = Hashtbl.create ~random:true 100
let add_typ id typ = Hashtbl.add main_tbl id typ
let get_typ id = try Hashtbl.find main_tbl id with _ -> TSTuple []

let print_table () =
  Hashtbl.iter
    (fun x y ->
      print_string (string_of_int x);
      print_string ": ";
      print_endline (Ast.pshow_typesig y))
    main_tbl
