open Ast
open Print_ast
open Typecheck
open Opt

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
  let t = Unix.gettimeofday() in
  begin
    let list = Array.to_list Sys.argv in
    let files = List.tl list in
    let programs = List.map parseToAst files in
    let toplevelslist =
      List.map (fun y -> match y with
                         | Prog x -> x) programs 
    in
    List.iter (typecheckAst) toplevelslist;
    let opt = optToplevelList toplevelslist in
    List.iter (List.iter (printToplevel 0)) opt
  end;
  Printf.printf "\nkhasmc done in %fs\n"  ((Unix.gettimeofday()) -. t)
