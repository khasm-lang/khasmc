open Share.Uuid
open Parsing.Ast
open Frontend.Typecheck
open Frontend.Monomorphize
open Parsing.Parser

let pp_unit fmt p = Format.fprintf fmt "()"
let r x = R x

let main () =
  Printexc.record_backtrace true;
  let file = Sys.argv.(1) in
  let s = In_channel.with_open_bin file In_channel.input_all in
  let lexbuf = Sedlexing.Utf8.from_string s in
  begin
    match toplevel lexbuf with
    | Error s ->
        print_endline "noooo it failed :despair:";
        print_endline s
    | Ok e ->
        print_endline "parsed:";
        List.iter
          (fun x ->
            print_endline (show_toplevel pp_resolved pp_unit x))
          e;
        print_endline "end\n";
        print_newline ();
        typecheck e;
        print_endline "raw type info:";
        Hashtbl.iter
          (fun nm ty ->
            print_endline
              (show_resolved nm ^ " : " ^ show_typ pp_resolved ty))
          raw_type_information;
        let ctx, after_mono = monomorphize e in
        print_endline "mono'd:";
        List.iter
          (fun x ->
            print_endline (show_toplevel pp_resolved pp_unit x))
          after_mono;
        print_endline "\nctx:";
        print_endline (show_monomorph_ctx ctx);
        let p_compiled =
          Frontend.Pattern_match_desugar.pattern_match_desugar
            after_mono
        in
        List.iter
          (fun x ->
            print_endline (show_toplevel pp_resolved pp_unit x))
          p_compiled
  end;
  print_endline "done"
