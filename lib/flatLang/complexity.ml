open IR

let all_defs = true

let rec complexity_e (expr : expr) =
  let go = complexity_e in
  match expr with
  Expr (_, _, kids) ->
    1 + List.fold_left (+) 0 (List.map go kids)

let complexity_def (def : definition) = complexity_e def.body

let complexity_verbose (prog : program) =
  if all_defs then 
    List.iter (fun def ->
      print_string ("complexity of def: " ^ show_name def.name);
      print_endline (" = " ^ string_of_int @@ complexity_e def.body)
    ) prog.defs
  else
    print_string "total complexity = ";
  print_endline @@ string_of_int @@ (List.fold_left (+) 0 @@
  List.map (fun def -> complexity_e def.body) prog.defs)

