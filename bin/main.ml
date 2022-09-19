open Print_ast
open Typecheck
open Opt
open Codegen


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
    begin
      let programs = List.map parseToAst files in
      begin
        List.iter (typecheckAst) programs;
        let opt = List.map (optProgram) programs in
        let code = codegenProgramList files programs in
        List.iter (printAst) opt;
        List.iter print_endline code
      end
    end
  end;
  Printf.printf "\nkhasmc done in %fs\n"  ((Unix.gettimeofday()) -. t)
