open Share.Types
open Share.Uuid
open Parsing.Ast
open Frontend.Typecheck
open Frontend.Monomorphize
open Parsing.Parser

let pp_unit fmt p = Format.fprintf fmt "()"
let r x = R (x, "(GEN)")
let data = data' ()

let contr =
  Module
    ( "A",
      [
        Module
          ( "B",
            [
              Definition
                {
                  data;
                  name = "b";
                  typeargs = [];
                  args = [];
                  return = TyBottom;
                  body = Just (Var (data, "bbody"));
                };
            ] );
        Module ("C", [ Open "B" ]);
      ] )
  :: []

let main () =
  begin
    Printexc.record_backtrace true;
    let file = Sys.argv.(1) in
    let s = In_channel.with_open_bin file In_channel.input_all in
    let lexbuf = Sedlexing.Utf8.from_string s in
    begin match toplevel lexbuf with
    | Error s ->
        print_endline "noooo it failed :despair:";
        print_endline s
    | Ok e ->
        print_endline "parsed:";
        List.iter
          (fun x ->
            print_endline
              (show_toplevel Format.pp_print_string pp_unit pp_unit x))
          e;
        print_endline "end\n";
        print_newline ();

        let e =
          if false then
            contr
          else
            e
        in

        let resolved = Parsing.Name_resolve.name_resolve e in

        print_endline "name resolved:";
        List.iter
          (fun x ->
            print_endline
              (show_toplevel pp_resolved pp_unit pp_unit x))
          resolved;

        typecheck resolved;
        print_endline "raw type info:";
        Hashtbl.iter
          (fun nm ty ->
            print_endline
              (show_resolved nm ^ " : " ^ show_typ pp_resolved ty))
          raw_type_information;
        let ctx, after_mono = monomorphize resolved in
        print_endline "mono'd:";
        List.iter
          (fun x ->
            print_endline
              (show_toplevel pp_resolved pp_unit pp_void x))
          after_mono;
        print_endline "\nctx:";
        print_endline (show_monomorph_ctx ctx);
        let p_comp =
          Frontend.Pattern_match_desugar.pattern_match_desugar
            after_mono
        in
        print_endline "pattern compiled:";
        List.iter
          (fun x ->
            print_endline
              (show_toplevel pp_resolved pp_unit pp_void x))
          p_comp
    end;
    print_endline "done"
  end
