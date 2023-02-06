let dast1 = ref false
let dast2 = ref false
let ins : string list ref = ref []
let outs : string ref = ref ""
let usage = "khasmc [--dump-ast1] [--dump-ast2] <file1> [<file2>] ... -o output"

type cliargs = {
  dump_ast1 : bool;
  dump_ast2 : bool;
  files : string list;
  out : string;
}
[@@deriving show { with_path = false }]

let generic s = ins := s :: !ins
