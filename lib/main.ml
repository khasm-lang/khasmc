open Batteries
open Lexing
open Uniq_typevars
open Typecheck
open Exp
open Ast
open Hash
open Process
open Args

let read_file file = BatFile.with_file_in file BatIO.read_all

let parseToAst filename =
  let file = read_file filename in
  let lexbuf = Lexing.from_string file in
  Lexing.set_filename lexbuf filename;
  let result = Parser.program Lexer.token lexbuf file in
  result

let speclist =
  [
    ("--dump-ast1", Arg.Set dast1, "Dump first AST");
    ("--dump-ast2", Arg.Set dast2, "Dump second AST");
    ("-o", Arg.Set_string outs, "Output file");
  ]

let parse_args () =
  Arg.parse speclist generic usage;
  { dump_ast1 = !dast1; dump_ast2 = !dast2; files = !ins; out = !outs }

let main_proc () =
  Printexc.record_backtrace true;
  let args = parse_args () in
  let succ =
    try
      let files = args.files in
      (* make this better *)
      let programs = List.map parseToAst files in
      let names = normalise files in
      let res = compile names programs args in
      print_endline res;
      "Success"
    with
    | TypeErr x -> "Caught TypeErr:\n" ^ x
    | NotFound x -> "Caught NotFound:\n" ^ x
    | NotImpl x -> "NOTIMPL:\n" ^ x
    | UnifyErr x -> "Caught UnifyErr:\n" ^ x
    | Impossible x -> "IMPOSSIBLE: " ^ x
  in
  Debug.debug ("\nStatus: " ^ succ);
  Debug.debug
    ("Used " ^ string_of_int !uniq ^ " typvars, " ^ string_of_int !muniq
   ^ " metavars and "
    ^ string_of_int (getid () - 1)
    ^ " nodes");
  Hash.print_table ();
  if succ <> "Success" then Debug.log_debug_stdout true else ()
