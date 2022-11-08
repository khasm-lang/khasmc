let parseToAst filename =
  let file = open_in filename in
  let lexbuf = Lexing.from_channel file in
  try
    let result = Parser.program Lexer.token lexbuf in
    close_in file;
    result
  with Parser.Error(x) ->
        begin
          print_string "failed in state: ";
          print_string (string_of_int x);
          print_endline "";
          exit 1
        end

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
    let as_str = List.map Ast.show_program programs in
    List.iter print_endline as_str
  end;
  Printf.printf "\nkhasmc done in %fs\n"  ((Unix.gettimeofday()) -. t)
