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


let dast1 = ref false
let dast2 = ref false
let ins = [] ref
let outs = "" ref

let usage = "khasmc [-dump_ast1] [-dump_ast2] <file1> [<file2>] ... -o output"

type cliargs = {
    dump_ast1: bool;
    dump_ast2: bool;
    files: string list;
    out: string;
  }

let speclist =
  [
    ("-dump_ast1", Arg.set dast1, "Dump first AST");
    ("-dump_ast2", Arg.set dast2, "Dump second AST");
    ("-o", Arg.set_string outs, "Output file");
  ]
let generic s = ins := s :: !ins

let parse_args () = 
  Arg.parse speclist generic usage;
  {
    dump_ast1 = !dast1;
    dump_ast2 = !dast2;
    files: !ins;
    out: !outs;
  }


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
