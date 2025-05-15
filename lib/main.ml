open Share.Uuid
open Frontend.Ast
open Frontend.Typecheck
open Frontend.Parser
open Frontend.Trait_resolution

let r x = R x

let main () =
  test ();
  (* print_endline "hey hey"; *)
  let file = Sys.argv.(1) in
  let s = In_channel.with_open_bin file In_channel.input_all in
  (* print_endline "file:";
  print_endline s; *)
  let lexbuf = Sedlexing.Utf8.from_string s in
  begin
    match toplevel lexbuf with
    | Ok e ->
        print_endline "parsed:";
        List.iter
          (fun x -> print_endline (show_toplevel pp_resolved x))
          e;
        print_endline "end\n";

        typecheck e;
        (*
      print_endline "\ntypes:";
      print_by_uuid (show_typ pp_resolved) type_information;
      print_endline "----------------\n";
       *)
        begin
          match resolve e with
          | Ok () -> ()
          | Error e -> print_endline e
        end
    | Error s ->
        print_endline "noooo it failed :despair:";
        print_endline s
  end;
  print_endline "done"
