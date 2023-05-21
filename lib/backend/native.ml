open Exp
open Args

let gen_main () =
  {|
int main(void) {
  kha_obj * empty = make_tuple(0);
  kha_obj * ret = main_____Khasm(1, &empty);
  if (ret->tag != TUPLE) {
  fprintf(stderr, "RETURN VALUE NOT TUPLE - TYPE SYSTEM INVALID\n");
  }  
  unref(empty);
  unref(ret);  
  return 0;  
}
|}

let prelude () =
  {|
#include <stdio.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdlib.h>
|}

let flags =
  {| -O3 -g -fsanitize=address -fno-omit-frame-pointer -Wall -Wextra -L/usr/lib/ -lgc |}

let compile code (args : Args.cliargs) =
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
      FileUtil.rm [ filename ];
      Sys.rmdir tmpdir;
      ()
  | _ -> raise @@ Impossible "non-standard os"
