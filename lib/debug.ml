let debug_s = ref ""
let debug str = debug_s := !debug_s ^ "\n" ^ str
let log_debug_stdout b = if b then print_endline !debug_s else ()

let log_debug_destruct () =
  print_endline !debug_s;
  debug_s := ""
