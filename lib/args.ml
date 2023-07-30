let dast1 = ref false
let dast2 = ref false
let dast3 = ref false
let dast4 = ref false
let debug = ref false
let table = ref false
let ins : string list ref = ref []
let outs : string ref = ref "a.out"

let usage =
  "khasmc [--dump-ast1] [--dump-ast2] [--dump-ast3] [--dump-ast4] [--debug] \
   [--table] <file1> [<file2>] ... -o output"

type cliargs = {
  dump_ast1 : bool;
  dump_ast2 : bool;
  dump_ast3 : bool;
  dump_ast4 : bool;
  files : string list;
  out : string;
  debug : bool;
  table : bool;
}
[@@deriving show { with_path = false }]

let generic s = ins := s :: !ins
