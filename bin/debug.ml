let debug_s = ref ""

let debug str = debug_s := !debug_s ^ "\n" ^ str

let log_debug_stdout () = print_endline !debug_s
