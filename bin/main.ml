open Batteries
open Lexing
open Uniq_typevars
open Typecheck
open Exp
open Ast
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
  with
  | Parser.Error(x) ->
     let error_msg = Fmt.str "%s: syntax error in state %d@." (print_error_position lexbuf) x in
     print_endline ("Parse error: " ^ error_msg);
     exit 1

let rec normalise files =
  match files with
  | [] -> []
  | x :: xs ->
     (
       Filename.basename x
       |> Filename.chop_extension
       |> BatString.captialize
     )  :: normalise xs




let _ =
  let () = Printexc.record_backtrace true in
  let argc = Array.length Sys.argv in
  if argc < 2 then
    begin
      print_endline "USAGE: ./khasmc <file>";
      exit 1
    end;
  print_endline "";
  let t = Unix.gettimeofday() in
  let succ = begin
    try
      let list = Array.to_list Sys.argv in
      let files = List.tl list in (* make this better *)
      let programs = List.map parseToAst files in
      let names = normalise files in 
      (*
        After the following, all code is assumed to have correct types.
       *)
      typecheck_program_list programs;
      let out = Codegen_lua.codegen names programs in
      let fp = open_out "out.lua" in
      Printf.fprintf fp "%s" out;
      "success"
    with
    | TypeErr(x) -> ("Caught TypeErr:\n" ^ x)
    | NotFound(x) -> ("Caught NotFound:\n" ^ x)
    | NotImpl(x) -> ("NOTIMPL:\n" ^ x)
    | UnifyErr(x) -> ("Caught UnifyErr:\n" ^ x)
    end
  in
  Debug.log_debug_stdout true;
  print_endline ("\nStatus: " ^ succ);
  Printf.printf "\nkhasmc done in %fs\n"  ((Unix.gettimeofday()) -. t);
  ()
