open Ast

let do_frontend (files : file list) : statement list =
  let without_modules = Modules.handle_files files in
  List.iter (fun x -> print_string @@ show_file x) without_modules;
  print_newline ();
  []
