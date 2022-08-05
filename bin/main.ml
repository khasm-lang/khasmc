
open Parser
open Print_ast
open Typecheck

let rec do_all f lst =
  match lst with
  | [] -> ()
  | x :: xs -> f x; do_all f xs

let parseToAst filename =
  let file = open_in filename in
  let lexbuf = Lexing.from_channel file in
  let result = Parser.program Lexer.token lexbuf in
  close_in file;
  result

let _ =
  let argc = Array.length Sys.argv in
  if argc < 2 then
    begin
      print_endline "USAGE: ./khasmc <file>";
      exit 1
    end;
  let list = Array.to_list Sys.argv in
  let files = List.tl list in
  let programs = List.map parseToAst files in
  do_all typecheckAst programs;
  do_all printAst programs

