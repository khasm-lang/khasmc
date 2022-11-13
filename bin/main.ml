
open Lexing

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




let _ =
  let argc = Array.length Sys.argv in
  if argc < 2 then
    begin
      print_endline "USAGE: ./khasmc <file>";
      exit 1
    end;
  print_endline "";
  let t = Unix.gettimeofday() in
  begin
    let list = Array.to_list Sys.argv in
    let files = List.tl list in
    let programs = List.map parseToAst files in
    let as_str = List.map Ast.show_program programs in
    List.iter print_endline as_str
  end;
  Printf.printf "\nkhasmc done in %fs\n"  ((Unix.gettimeofday()) -. t);
  let test = Typecheck_env.unify
               (
                 TSForall(
                     ["'b"],
                     (TSBase(KTypeBasic("'a")))
                   )
               )
               (TSMap(TSBase(KTypeBasic("a")),TSBase(KTypeBasic("a"))))
               (Typecheck_env.new_unity None)
  in 
  print_endline (Typecheck_env.show_unity test)
