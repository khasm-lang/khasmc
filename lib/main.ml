open Batteries
open Lexing
open Lexer
open Typecheck
open Uniq_typevars
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
    ("--dump-ast3", Arg.Set dast3, "Dump third AST");
    ("--dump-ast4", Arg.Set dast4, "Dump fourth AST");
    ("--table", Arg.Set table, "Show type table");
    ("-o", Arg.Set_string outs, "Output file");
    ("--debug", Arg.Set debug, "Debug");
    ("--no-compile", Arg.Set nocomp, "Don't compile the program");
    ("-O", Arg.Set_int opt, "Optimisation level");
  ]

let parse_args () =
  Arg.parse speclist generic usage;
  {
    dump_ast1 = !dast1;
    dump_ast2 = !dast2;
    files = !ins;
    out = !outs;
    dump_ast3 = !dast3;
    dump_ast4 = !dast4;
    debug = !debug;
    table = !table;
    nocompile = !nocomp;
    opt = !opt;
  }

let round2 n = Float.round (n *. 100.) /. 100.

let main_proc () =
  let t = Unix.gettimeofday () in
  Printexc.record_backtrace true;
  Random.self_init ();
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
    with Lexer.EOF x -> "EOF:\n" ^ x
  in
  Debug.debug ("\nStatus: " ^ succ);
  print_endline
    ("Used "
    ^ string_of_int !uniq
    ^ " typvars, "
    ^ string_of_int !muniq
    ^ " metavars, "
    ^ string_of_int (getid () - 1)
    ^ " stage 1 nodes, "
    ^ string_of_int (Kir.get_random_num () - 1)
    ^ " stage 2 nodes.");
  print_endline
    ("Took: "
    ^ string_of_float (round2 @@ (Unix.gettimeofday () -. t))
    ^ " seconds.");
  if args.table then
    Hash.print_table ()
  else
    ();
  if args.debug || succ <> "Success" then
    Debug.log_debug_stdout true
  else
    ()
