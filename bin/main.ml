
open Lexing
open Uniq_typevars
open Typecheck
open Exp
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
       |> BatString.capitalize
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
  begin
    try
      let list = Array.to_list Sys.argv in
      let files = List.tl list in (* make this better *)
      let programs = List.map parseToAst files in
      let names = normalise files in 
      let uniq_typevars = List.map make_uniq_typevars programs in
      (*
        After the following, all code is assumed to have correct types.
       *)
      typecheck_program_list uniq_typevars;
      let out = Codegen_lua.codegen names uniq_typevars in
      let fp = open_out "out.lua" in
      Printf.fprintf fp "%s" out;
      ()
    with
    | TypeErr(x) -> print_endline ("Caught TypeErr:\n" ^ x)
    | NotFound(x) -> print_endline ("Caught NotFound:\n" ^ x)
    | NotImpl(x) -> print_endline ("NOTIMPL:\n" ^ x)
    | UnifyErr(x) -> print_endline ("Caught UnifyErr:\n" ^ x)
  end;
  (* Debug.log_debug_stdout (); *)
  Printf.printf "\nkhasmc done in %fs\n"  ((Unix.gettimeofday()) -. t)
