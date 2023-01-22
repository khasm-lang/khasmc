open Batteries
open Lexing
open Uniq_typevars
open Typecheck
open Exp
open Ast
open Hash

let print_error_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Fmt.str "Line: %d, Position: %d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parseToAst filename =
  let file = open_in filename in
  let lexbuf = Lexing.from_channel file in
  try
    let result = Parser.program Lexer.token lexbuf in
    close_in file;
    result
  with Parser.Error x ->
    let error_msg =
      Fmt.str "%s: syntax error in state %d@." (print_error_position lexbuf) x
    in
    print_endline ("Parse error: " ^ error_msg);
    exit 1

let rec normalise files =
  match files with
  | [] -> []
  | x :: xs ->
      (Filename.basename x |> Filename.chop_extension |> BatString.capitalize)
      :: normalise xs

let compile names asts =
  let asts' = asts |> List.map Complexity.init_program in

  (*typcheck stage*)
  asts' |> List.map Typelam_init.init_program |> typecheck_program_list;
  (*codegen stage*)
  asts' |> List.iter (fun x -> Debug.debug (show_program x));
  asts |> Backend.codegen names

let main_proc () =
  let () = Printexc.record_backtrace true in
  let argc = Array.length Sys.argv in
  if argc < 2 then (
    print_endline "USAGE: ./khasmc <file>";
    exit 1);
  print_endline "";
  let t = Unix.gettimeofday () in
  let succ =
    try
      let list = Array.to_list Sys.argv in
      let files = List.tl list in
      (* make this better *)
      let programs = List.map parseToAst files in
      let names = normalise files in
      let res = compile names programs in
      print_endline res;
      "Success"
    with
    | TypeErr x -> "Caught TypeErr:\n" ^ x
    | NotFound x -> "Caught NotFound:\n" ^ x
    | NotImpl x -> "NOTIMPL:\n" ^ x
    | UnifyErr x -> "Caught UnifyErr:\n" ^ x
  in

  Debug.debug ("\nStatus: " ^ succ);
  Debug.debug
    ("Used " ^ string_of_int !uniq ^ " typvars, " ^ string_of_int !muniq
   ^ " metavars and "
    ^ string_of_int (getid () - 1)
    ^ " nodes");
  if succ <> "Success" then Debug.log_debug_stdout true else ()
