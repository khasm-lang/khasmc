(** The main process of compilation *)

open Args

let rec normalise files =
  match files with
  | [] -> []
  | x :: xs -> (
      try
        (Filename.basename x |> Filename.chop_extension
       |> Batteries.String.capitalize_ascii)
        :: normalise xs
      with _ -> Batteries.String.capitalize_ascii x :: normalise xs)

let compile names asts args =
  print_endline "";

  if args.dump_ast1 then
    asts |> List.iter (fun x -> print_endline (Ast.show_program x))
  else
    ();

  let asts' =
    asts
    (* parsing puts them in reverse order,
            so fix that *)
    |> List.rev
    |> List.map Complexity.init_program
    |> List.map2 Modules.wrap_in (List.rev names)
    |> Elim_modules.elim
  in

  (* After here, program must have no modules remaining *)
  if args.dump_ast2 then
    asts' |> List.iter (fun x -> print_endline (Ast.show_program x))
  else
    ();

  (*typcheck stage*)
  asts' |> Typecheck.typecheck_program_list;

  (* After here, program should be type correct *)

  (*codegen stage*)
  let kir = asts' |> Translateftm.front_to_middle in
  let kir' = kir |> Lamlift.lambda_lift in

  (* After here, program should have no lambdas or closures *)
  if args.dump_ast3 then
    print_endline (Kir.show_kirprog kir')
  else
    ();

  let khagm = kir' |> Translatemtb.mtb in
  if args.dump_ast4 then (
    print_endline "KHAGM BEFORE OPTS:\n";
    print_endline (Khagm.show_khagm khagm));
  let opt =
    khagm |> Khagm_elim_dups.elim_dups |> Remove_zeroarity.remove_zeroarity
    |> Insert_refcounts.insert_refcounts
  in
  if args.dump_ast4 then (
    print_endline "\n\n\nKHAGM AFTER OPTS:\n";
    print_endline (Khagm.show_khagm opt));
  let out = opt |> Emit_c.emit_c in
  if not args.nocompile then (
    Native.compile out args;
    "Output as: " ^ args.out)
  else
    "Not output"
