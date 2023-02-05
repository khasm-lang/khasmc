let rec normalise files =
  match files with
  | [] -> []
  | x :: xs ->
      (Filename.basename x |> Filename.chop_extension
     |> Batteries.String.capitalize)
      :: normalise xs

let compile names asts =
  let asts' =
    asts |> List.map Complexity.init_program |> List.map2 Modules.wrap_in names
  in

  (*typcheck stage*)
  asts'
  |> List.map Typelam_init.init_program
  |> Typecheck.typecheck_program_list;
  (*codegen stage*)
  asts' |> List.iter (fun x -> Debug.debug (Ast.show_program x));
  asts' |> Translateftm.front_to_middle |> fun x ->
  print_endline (Kir.show_kirprog x);
  "done"
