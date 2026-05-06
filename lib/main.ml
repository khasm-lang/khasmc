open Share.Types
open Share.Uuid
open Parsing.Ast
open Parselang.Typecheck
open Parselang.Monomorphize
open Parsing.Parser

let pp_unit fmt p = Format.fprintf fmt "()"
let r x = R x
let data = data' ()

let debug_parse = true
let debug_flat = true
let debug_flat_small = true
let time = true
let debug_gc = false

let with_timer name thunk =
  if time then
    print_endline ("\nstarting: " ^ name);
  let start = Unix.gettimeofday () in
  let res = thunk () in
  let stop = Unix.gettimeofday () in
  if time then begin
    print_endline ("done: " ^ name);
    Printf.printf "took: %4fs\n" (stop -. start)
  end;
  if time then
    print_newline ();
  res

let main () =
  with_timer "khasmc"
    begin
      fun () ->
        Printexc.record_backtrace true;
        let file = Sys.argv.(1) in
        let s = In_channel.with_open_bin file In_channel.input_all in
        let lexbuf = Sedlexing.Utf8.from_string s in
        begin
          match with_timer "parser" (fun () -> toplevel lexbuf) with
          | Error s ->
              print_endline "noooo it failed :despair:";
              print_endline s
          | Ok e ->
              if debug_parse then begin
                print_endline "parsed:";
                List.iter
                  (fun x ->
                    print_endline
                      (show_toplevel Format.pp_print_string pp_unit
                         pp_unit x))
                  e;
                print_endline "end\n";
                print_newline ()
              end;

              let resolved =
                with_timer "name resolution" (fun () ->
                    Parsing.Name_resolve.name_resolve e)
              in

              if debug_parse then begin
                print_endline "name resolved:";
                List.iter
                  (fun x ->
                    print_endline
                      (show_toplevel pp_resolved pp_unit pp_unit x))
                  resolved
              end;

              with_timer "typechecking" (fun () -> typecheck resolved);

              if debug_parse then begin
                print_endline "raw type info:";
                Hashtbl.iter
                  (fun nm ty ->
                    print_endline
                      (show_resolved nm
                      ^ " : "
                      ^ show_typ pp_resolved ty))
                  ident_type_info
              end;

              let ctx, after_mono =
                with_timer "monomorphization" (fun () ->
                    monomorphize resolved)
              in

              if debug_parse then begin
                print_endline "mono'd:";
                List.iter
                  (fun x ->
                    print_endline
                      (show_toplevel pp_resolved pp_unit pp_void x))
                  after_mono;
                print_endline "\nctx:";
                print_endline (show_monomorph_ctx ctx)
              end;

              let p_comp =
                with_timer "pattern compilation" (fun () ->
                    Parselang.Pattern_match_desugar
                    .pattern_match_desugar after_mono)
              in

              if debug_parse then begin
                print_endline "pattern compiled:";
                List.iter
                  (fun x ->
                    print_endline
                      (show_toplevel pp_resolved pp_unit pp_void x))
                  p_comp
              end;

              let to_flat =
                with_timer "to flatland" (fun () ->
                    Flatlang.From_parselang.conv_top p_comp)
              in

              if debug_flat then begin
                print_endline "to flatlang";
                print_endline (Flatlang.IR.show_program to_flat);
              end;
              if debug_flat_small then
              if not (Flatlang.Verify.verify true to_flat) then
                print_endline "DID NOT VERIFY"
              else
                ();

              let with_types_again =
                with_timer "reconstruct types" (fun () ->
                  Flatlang.Reconstruct_types.reconstruct to_flat
                )
              in
              if debug_flat then begin
                print_endline "types reconstructed print ommitted";
              end;
              if debug_flat_small then
                if not (Flatlang.Verify.verify true with_types_again) then
                  print_endline "DID NOT VERIFY"
                else
                  ();
                  
              let clos_conved =
                with_timer "closure convert" (fun () ->
                  Flatlang.Closure_convert.clos_conv with_types_again
                )
              in
              
              if debug_flat then begin
                print_endline "clos converted:";
                print_endline (Flatlang.IR.show_program clos_conved);
              end;

              if debug_flat_small then
                if not (Flatlang.Verify.verify true clos_conved) then
                  print_endline "DID NOT VERIFY"
                else
                  ();

              let with_types_again2 =
                with_timer "reconstruct types 2" (fun () ->
                  Flatlang.Reconstruct_types.reconstruct clos_conved
                )
              in
              if debug_flat then begin
                print_endline "types reconstructed 2 print ommitted";
              end;
              if debug_flat_small then
                if not (Flatlang.Verify.verify true with_types_again2) then
                  print_endline "DID NOT VERIFY"
                else
                  ();

              let let_folded =
                with_timer "let fold" (fun () ->
                    Flatlang.Let_fold.let_fold with_types_again2)
              in
              
              if debug_flat then begin
                print_endline "let folded:";
                print_endline (Flatlang.IR.show_program let_folded);
              end;

              if debug_flat_small then

              if not (Flatlang.Verify.verify true let_folded) then
                print_endline "DID NOT VERIFY"
              else
                ();
              ()
        end
    end;
  if debug_gc then begin
    let stat = Gc.stat () in
    let i = string_of_int in
    let f = string_of_float in
    let mb = 1024 * 1024 in
    print_endline
      ("minor words: "
      ^ f stat.minor_words
      ^ " ("
      ^ i (Float.to_int stat.minor_words / mb)
      ^ "mb)");
    print_endline
      ("major words: "
      ^ f stat.major_words
      ^ " ("
      ^ i (Float.to_int stat.major_words / mb)
      ^ "mb)");
    print_endline ("minor colls: " ^ i stat.minor_collections);
    print_endline ("major colls: " ^ i stat.major_collections);
    print_endline
      ("top heap words: "
      ^ i stat.top_heap_words
      ^ " ("
      ^ i (stat.top_heap_words / mb)
      ^ "mb)")
  end
