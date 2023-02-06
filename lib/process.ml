open Args

let rec normalise files =
  match files with
  | [] -> []
  | x :: xs ->
      (Filename.basename x |> Filename.chop_extension
     |> Batteries.String.capitalize)
      :: normalise xs

let compile names asts args =
  let asts' =
    asts |> List.map Complexity.init_program |> List.map2 Modules.wrap_in names
  in

  if args.dump_ast1 then
    asts' |> List.iter (fun x -> print_endline (Ast.show_program x))
  else ();

  (*typcheck stage*)
  asts'
  |> List.map Typelam_init.init_program
  |> Typecheck.typecheck_program_list;

  (*codegen stage*)
  let kir = asts' |> Translateftm.front_to_middle in

  if args.dump_ast2 then print_endline (Kir.show_kirprog kir) else ();

  "done"
