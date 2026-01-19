open Share.Uuid
open Parsing.Ast
open Frontend.Typecheck
open Frontend.Monomorphize
open Parsing.Parser

let pp_unit fmt p = Format.fprintf fmt "()"
let r x = R (x, "(GEN)")

let test () =
  let d = { uuid = uuid_using TyBottom; counter = 0; span = None } in
  print_endline "\n\n\n TEST:";
  let ast =
    [
      Definition
        {
          data = d;
          name = r "p";
          typeargs = [];
          args = [];
          return = TyBottom;
          body =
            Just
              (Match
                 ( d,
                   Var (d, r "expr"),
                   [
                     ( CaseTuple
                         [
                           CaseTuple
                             [
                               CaseTuple
                                 [
                                   CaseTuple [ CaseLit (LBool true) ];
                                 ];
                             ];
                         ],
                       Var (d, r "one") );
                     (CaseVar (r "otherwise"), Var (d, r "two"));
                   ] ));
        };
    ]
  in
  print_endline "\nbefore:";
  List.iter
    (fun x -> print_endline (show_toplevel pp_resolved pp_unit x))
    ast;
  print_endline "\n\n";
  let p_comp =
    Frontend.Pattern_match_desugar.pattern_match_desugar ast
  in
  print_endline "\npattern compiled:";
  List.iter
    (fun x -> print_endline (show_toplevel pp_resolved pp_unit x))
    p_comp

let main () =
  if false then
    test ()
  else begin
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
              (show_toplevel Format.pp_print_string pp_unit x))
          e;
        print_endline "end\n";
        print_newline ();

        let resolved = Parsing.Name_resolve.name_resolve e in

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
            print_endline (show_toplevel pp_resolved pp_unit x))
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
            print_endline (show_toplevel pp_resolved pp_unit x))
          p_comp
    end;
    print_endline "done"
  end
