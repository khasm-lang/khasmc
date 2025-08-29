open Share.Uuid
open Frontend.Ast
open Frontend.Typecheck
open Frontend.Parser
open Trait_resolution.Resolve

let r x = R x

let main () =
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
        print_newline ();
        typecheck e;
        (*
      print_endline "\ntypes:";
      print_by_uuid (show_typ pp_resolved) type_information;
      print_endline "----------------\n";
       *)
        begin
          match resolve e with
          | Ok () -> ()
          | Error e ->
              print_endline e;
              failwith "trait resolution bonked"
        end;

        print_endline "\n\ntrait info:\n";
        Hashtbl.iter
          (fun uuid t ->
            print_string "uuid: ";
            print_endline (show_uuid uuid);
            List.iter
              (fun a -> print_endline (" inst: " ^ show_solved a))
              t;
            ())
          trait_information;
        (*
        print_string "\n\ntype info:\n";
        Hashtbl.iter (fun a b ->
            print_endline ("uuid: " ^ show_uuid a);
            print_endline ("  type: " ^ show_typ pp_resolved b);
            ) type_information;
         *)
        let mono'd = Monomorph.Monomorphize.monomorphize e in
        print_endline "monomorph in progress";
        Monomorph.Monomorphize.print_monomorph_info mono'd
    | Error s ->
        print_endline "noooo it failed :despair:";
        print_endline s
  end;
  print_endline "done"
