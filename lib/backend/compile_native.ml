open Emit_c
open Exp

let run cmd =
  let inp = Unix.open_process_in cmd in
  let r = In_channel.input_all inp in
  In_channel.close inp;
  r

let gen_main () =
  {|
int main(void) {
  GC_INIT();  
  khagm_obj * m = main_____Khasm(create_list(1, create_tuple(NULL, 0)), 1);
  khagm_obj * end = khagm_whnf(m);
  printf("DIFF: %d\n", alloc_free_diff());
}
|}

let prelude () =
  {|
#include <stdio.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdlib.h>
#include "gc.h"  
|}

let flags =
  {| -O3 -g -fsanitize=address -fno-omit-frame-pointer -w -L/usr/lib/ -lgc |}

let to_native code (args : Args.cliargs) =
  let code = prelude () ^ Runtime_lib.runtime_c ^ code ^ gen_main () in
  match Sys.os_type with
  | "Win32" | "Cygwin" -> raise @@ NotSupported "Windows"
  | "Unix" ->
      let tmpdir =
        Filename.get_temp_dir_name ()
        ^ "/khasm_workdir_"
        ^ (Random.int 1000000 |> string_of_int)
      in
      Sys.mkdir tmpdir 0o755;
      let filename = tmpdir ^ "/khagm.c" in
      let oc = open_out filename in
      Printf.fprintf oc "%s\n" code;
      close_out oc;
      let code' = Sys.command ("cc " ^ flags ^ filename ^ " -o " ^ args.out) in
      FileUtil.cp [ filename ] ("./" ^ args.out ^ ".c");
      (match code' with 0 -> () | _ -> raise @@ CompileError "CC failed");
      ()
  | _ -> raise @@ Impossible "non-standard os"
