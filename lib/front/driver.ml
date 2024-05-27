open Ast

let do_frontend (files : file list) : statement list =
  let without_modules = Modules.handle_files files in
  let statements =
    List.map (fun f -> f.toplevel) without_modules |> List.flatten
  in
  let compressed_paths =
    Convert_idents_to_strings.convert statements
  in
  let tycheckd = Tycheck.typecheck compressed_paths in
  []
