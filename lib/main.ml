open Batteries
open Lexing
open Uniq_typevars
open Typecheck
open Exp
open Ast
open Hash
open Process

let read_file file = BatFile.with_file_in file BatIO.read_all

let parseToAst filename =
  let file = read_file filename in
  let lexbuf = Lexing.from_string file in
  Lexing.set_filename lexbuf filename;
  let result = Parser.program Lexer.token lexbuf file in
  result

let main_proc () =
  let () = Printexc.record_backtrace true in
  let argc = Array.length Sys.argv in
  if argc < 2 then (
    print_endline "USAGE: ./khasmc <file>";
    exit 1);
  print_endline "";
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
    | _ -> "Caught Something :P"
  in

  Debug.debug ("\nStatus: " ^ succ);
  Debug.debug
    ("Used " ^ string_of_int !uniq ^ " typvars, " ^ string_of_int !muniq
   ^ " metavars and "
    ^ string_of_int (getid () - 1)
    ^ " nodes");
  Hash.print_table ();
  if succ <> "Success" then Debug.log_debug_stdout true else ()
