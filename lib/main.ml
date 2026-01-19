open Share.Uuid
open Parsing.Ast
open Frontend.Typecheck
open Frontend.Monomorphize
open Parsing.Parser

let pp_unit fmt p = Format.fprintf fmt "()"
let r x = R x

let test () =
  let d = { uuid = uuid_using TyBottom; counter = 0; span = None } in
  print_endline "\n\n\n TEST:";
  let ast =
    [
      Definition
        {
          data = d;
          name = R "p";
          typeargs = [];
          args = [];
          return = TyBottom;
          body =
            Just
              (Match
                 ( d,
                   Var (d, R "expr"),
                   [
                     ( CaseCtor (R "Some", [ CaseLit (LBool true) ]),
                       Var (d, R "fst") );
                     ( CaseCtor (R "Some", [ CaseVar (R "x") ]),
                       Var (d, R "snd") );
                     (CaseCtor (R "None", []), Var (d, R "third"));
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
  if true then
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
